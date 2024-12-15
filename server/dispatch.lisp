;;;; Copyright 2022-2024 by Tomáš Zellerin

(in-package :http2)

(defsection @server
    (:title "Running a server")
  "To create a server, two major definitions are needed:

- what content to serve based on the client request (this is closely http2 related), and
- how to dispatch requests from the (possibly multiple) clients - this is mostly unrelated
  to http2, but still need to be decided.

The first question - how to handle requests - is handled in the example servers by DISPATCHER-MIXIN.

The second question - how to handle incoming TCP connections - has several implementations that can be accessed with a single interface, "
  (dispatcher-mixin class)
  (define-prefix-handler mgl-pax:macro)
  (define-exact-handler mgl-pax:macro)
  (send-text-handler function)
  (redirect-handler function)
  (@server/threaded section))

(defclass dispatcher-mixin ()
  ((exact-handlers  :accessor get-exact-handlers  :initarg :exact-handlers)
   (prefix-handlers :accessor get-prefix-handlers :initarg :prefix-handlers))
  (:default-initargs :exact-handlers nil :prefix-handlers nil)
  (:documentation
   "Server with behaviour that is defined by two sets of handlers, exact and
prefix. Appropriate handler is run to process the request when peer closes the
http2 stream. The exact handler must match fully the path (so not the query),
prefix handlers matches when the path starts with the prefix.

Protocol and domain are not checked. The behaviour is implemented in the
appropriate PEER-ENDS-HTTP-STREAM method.

The handlers are set using DEFINE-PREFIX-HANDLER or DEFINE-EXACT-HANDLER, and
are functions typically created by HANDLER macro, or (in simple cases) by
REDIRECT-HANDLER or SEND-TEXT-HANDLER functions."))

(eval-when (:compile-toplevel :load-toplevel)
  (defun define-some-handler (target prefix fn)
    `(setf ,target
           (acons ,prefix ,fn
                  (remove ,prefix ,target :key 'car :test 'equal)))))

(defmacro handler ((flexi-stream-name charset gzip) &body body)
  "Runs BODY in a context with
- FLEXI-STREAM-NAME bound to a flexi stream,
- and two available functions, SEND-HEADERS and SEND-GOAWAY to make a function
  that has suitable format for an exact or prefix handler; that it, that takes
  two parameters CONNECTION and (http2) STREAM and prepares response.

The SEND-HEADERS sends the provided headers to the STREAM.

The SEND-GOAWAY sends go away frame to the client to close connection.

It does NOT close the FLEXI-STREAM-NAME."
;  (warn "Handler macro is a relic no longer to use.")
  `(lambda (connection stream)
     (let ((,flexi-stream-name
             (make-transport-output-stream stream ,charset ,gzip)))
       (flet ((send-headers (&rest args)
                (apply #'send-headers stream args))
              (send-goaway (code debug-data)
                (write-goaway-frame connection
                                    0 code debug-data)
                (force-output (get-network-stream connection))))
         (declare (ignorable #'send-goaway))
         ,@body))))

(defmacro constant-handler ((flexi-stream-name charset gzip headers) &body body)
  "Run BODY to print the output to FLEXI-STREAM-NAME in compile time. This
constant (static) page is served every time as-is."
  `(let ((headers ,headers)
         (res (compile-payload-from-stream (,flexi-stream-name ,charset ,gzip)
                                           ,@body)))
     (when ,gzip
       (setf headers (append headers '(("content-encoding" "gzip")))))
     (lambda (connection stream)
       (send-headers stream headers)
       (write-binary-payload connection stream res))))

(defmacro scheduling-handler ((flexi-stream-name encoding gzip) &body body)
  "Version of HANDLER that is to be used for scheduled (or otherwise processed in
another thread) responses:
- It makes accessible in BODY function SCHEDULE that takes two parameters, delay in miliseconds and action to run after delay. See event stream implementation in the example server for the possible usage."
  `(lambda (connection stream)
     (let ((,flexi-stream-name (make-transport-output-stream stream
                                                             ,encoding ,gzip)))
       (flet ((send-headers (&rest args)
                (apply #'send-headers stream args))
              (send-goaway (code debug-data)
                (write-goaway-frame connection 0 code debug-data)
                (force-output (get-network-stream connection)))
              (schedule (delay action)
                (schedule-task-wake-thread (get-scheduler connection) delay action
                                           'scheduling-handler)
                'send-delayed))
         (declare (ignorable #'send-goaway #'schedule))
         ,@body))))

(defmacro define-prefix-handler (prefix fn &optional connection)
  "Define function to run when peer closes http stream on CONNECTION (or any
server defined in future) if the path of the stream starts with PREFIX."
  (define-some-handler (if connection
                           `(get-prefix-handlers connection) '*prefix-handlers*)
    prefix fn))

(defmacro define-exact-handler (path fn &optional connection)
  "Define function to run when peer closes http stream on CONNECTION (or any
server defined in future) if the path of the stream is PATH."
  (define-some-handler (if connection
                           `(get-exact-handlers connection) '*exact-handlers*)
    path fn))

(defvar *prefix-handlers*
  nil
  "Alist of prefixes and functions of connection and stream to make http response.")

(defvar *exact-handlers*
  ()
  "Alist of paths and functions of connection and stream to make http response.")

(defun send-text-handler (text &key (content-type "text/html; charset=UTF-8")
                                 (gzip t)
                                 additional-headers)
  "A handler that returns TEXT as content of CONTENT-TYPE.
ADDITIONAL-HEADERS are sent along with :status and content-type
headers."
  (let ((headers
          `((:status "200") ("content-type" ,content-type)
            ,@(when gzip '(("content-encoding" "gzip")))
            ,@additional-headers))
        (res
          ;; FIXME: use direct conversion, not streams
          (compile-payload-from-stream (out :utf-8 gzip)
            (princ text out))))
    (lambda (connection stream)
      (send-headers stream headers)
      (write-binary-payload connection stream res))))

(defun redirect-handler (target &key (code "301") (content-type "text/html; charset=UTF-8") content)
  "A handler that emits redirect response with http status being CODE, and
optionally provides CONTENT with CONTENT-TYPE."
  (handler (out :utf-8 nil)
    (with-open-stream (out out)
      (send-headers `((:status ,code)
                      ("location" ,target)
                      ,@(when content `(("content-type" ,content-type))))
                    :end-stream (null content))
      (when content
        (princ content out)))))

;;;; Sample server with constant payload
(defclass vanilla-server-connection (server-http2-connection
                                     dispatcher-mixin
                                     threaded-server-mixin
                                     stream-based-connection-mixin)
  ()
  (:default-initargs :stream-class 'vanilla-server-stream)
  (:documentation
   "A server connection that spawns streams of VANILLA-SERVER-STREAM type when a
new stream is requested, allows scheduled or other asynchronous writes, and
optionally prints activities."))

(defclass vanilla-server-stream (server-stream
                                 body-collecting-mixin
                                 multi-part-data-stream)
  ()
  (:documentation
   "A server-side stream that can be used as a binary output stream, optionally
prints activities, and reads full body from client if clients sends one."))

(defvar *default-handler*
  (constant-handler (out :utf-8 nil `((:status "404")
                                      ("content-type" "text/html; charset=UTF-8")))
    (format out  "<h1>Not found</h1>"))
  "Handler used as last resort - page not found.")

(defun find-matching-handler (path connection)
  "Function that should prepare response for request on PATH in streams of given
CONNECTION."
  (or
   (cdr (assoc path
               (or (get-exact-handlers connection)
                   *exact-handlers*)
               :test (lambda (prefix path)
                             (let ((mismatch (mismatch prefix path)))
                               (or (null mismatch)
                                   (and (eql mismatch (position #\? path))
                                        (eql mismatch (length path))))))))
   (cdr (assoc path
               (or (get-prefix-handlers connection)
                   *prefix-handlers*)
               :test (lambda (prefix path)
                       (let ((mismatch (mismatch prefix path)))
                         (or (null mismatch) (equal mismatch (length path)))))))
   *default-handler*))

(defmethod peer-ends-http-stream ((stream vanilla-server-stream))
  "Send appropriate payload, or an error page."
  (with-slots (connection) stream
    (funcall (find-matching-handler (get-path stream) connection) connection stream)))

(defmethod queue-frame :around ((server threaded-server-mixin) frame)
  (bt:with-lock-held ((get-lock server))
    (call-next-method)))

(defmethod maybe-lock-for-write ((c threaded-server-mixin))
  (error "Do not call me"))

(defmethod maybe-unlock-for-write ((c threaded-server-mixin))
  (error "Do not callme"))

(defgeneric cleanup-connection (connection)
  (:method (connection) nil)
  (:method :after ((connection threaded-server-mixin))
    (stop-scheduler-in-thread (get-scheduler connection)))
  (:documentation
   "Remove resources associated with a server connection. Called after connection is
closed."))

(defun process-server-stream (stream &key (connection-class 'vanilla-server-connection)
                                       connection)
  "Make a HTTP2 connection of CONNECTION-CLASS on the underlying STREAM (that is a
stream in Common Lisp sense, so either network stream or even standard io) and
read frames from it until END-OF-FILE (client closed the underlying stream - or
maybe we do) or GO-AWAY (client closes connection - or maybe we do) is
signalled."
  (let ((connection (or connection
                        (make-instance connection-class))))
    (with-simple-restart (close-connection "Close current connection")
      (handler-case
          (unwind-protect
               (progn
                 (setf (get-network-stream connection) stream)
                 (process-pending-frames connection nil #'parse-client-preface (length +client-preface-start+)))
            (cleanup-connection connection))
        (end-of-file ())))))
