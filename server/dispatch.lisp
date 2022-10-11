;;;; Copyright 2022 by Tomáš Zellerin

;;;; As an example of how could server be built, define class
;;;; vanilla-server-connection that dispatches incoming requests based on their
;;;; path to a handler function.  Handlers are mapped either to specific path
;;;; (exact handler) or to a prefix (prefix handler).
;;;;
;;;; use HANDLER macro to wrap handlers; it provides convenient and exported
;;;; function for the handler to use (SEND-DATA, SEND-TEXT, SEND-HEADERS,
;;;; SEND-GOAWAY).
;;;;
;;;; Of course, existing framework arount http/1.1 (Hunchentoot, ...) provide
;;;; much more. I would prefer not do duplicate.


(in-package :http2)

(defclass dispatcher-mixin ()
  ((exact-handlers  :accessor get-exact-handlers  :initarg :exact-handlers)
   (prefix-handlers :accessor get-prefix-handlers :initarg :prefix-handlers))
  (:default-initargs :exact-handlers nil :prefix-handlers nil)
  (:documentation
   "Server with behaviour that is defined by two sets of handlers, exact and
prefix. Appropriate handler is run to process the request when peer closes the
http2 stream. The exact handler must match fully the path (i.e., excluding
query), the path must start with the prefix handler to match.

Protocol and domain are not checked. The behaviour is implemented in the
appropriate PEER-ENDS-HTTP-STREAM method.

The handlers are set using DEFINE-PREFIX-HANDLER or DEFINE-EXACT-HANDLER, and
are functions typically created by HANDLER macro, or (in simple cases) by
REDIRECT-HANDLER or SEND-TEXT-HANDLER functions."))

(eval-when (:compile-toplevel :load-toplevel)
  (defun define-some-handler (target prefix fn)
    `(progn
       (setf ,target (remove ,prefix ,target :key 'car :test 'equal))
       (push (cons ,prefix ,fn) ,target))))

(defmacro handler ((flexi-stream-name &rest flexi-pars) &body body)
  "Runs BODY in a context with
- FLEXI-STREAM-NAME bound to a flexi stream,
- and two available functions, SEND-HEADERS and SEND-GOAWAY to make a function
  that has suitable format for an exact or prefix handler; that it, that takes
  two parameters CONNECTION and (http2) STREAM and prepares response.

The SEND-HEADERS sends the provided headers to the STREAM.

The SEND-GOAWAY sends go away frame to the client to close connection."
  `(lambda (connection stream)
     (with-open-stream (,flexi-stream-name (flexi-streams:make-flexi-stream
                                            stream
                             ,@flexi-pars))
       (flet ((send-headers (&rest args)
                (apply #'send-headers stream args))
              (send-goaway (code debug-data)
                (http2::write-goaway-frame connection
                                           0 code debug-data)
                (force-output (get-network-stream connection))))
         (declare (ignorable #'send-goaway))
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
                           `(get-prefix-handlers connection) '*exact-handlers*)
    path fn))

(defvar *prefix-handlers*
  nil
  "Alist of prefixes and functions of connection and stream to make http response.")

(defvar *exact-handlers*
  ()
  "Alist of paths and functions of connection and stream to make http response.")

(defun send-text-handler (text &key (content-type "text/html; charset=UTF-8")
                                 additional-headers)
  "A handler that returns TEXT as content of CONTENT-TYPE.
ADDITIONAL-HEADERS are sent along with :status and content-type
headers."
  (handler (out)
    (send-headers `((:status "200") ("content-type" ,content-type)
                    ,@additional-headers))
    (princ text out)))

(defun redirect-handler (target &key (code "301") (content-type "text/html; charset=UTF-8") content)
  "A handler that emits redirect response with http status being CODE, and
optionally provided CONTENT wit CONTENT-TYPE."
  (handler (out)
    (send-headers `((:status ,code)
                    ("location" ,target)
                    ,@(when content `(("content-type" ,content-type))))
                  :end-stream (null content))
    (when content
      (princ content  out))))

;;;; Sample server with constant payload
(defclass vanilla-server-connection (server-http2-connection
                                    dispatcher-mixin
                                    history-printing-object)
  ()
  (:default-initargs :stream-class 'vanilla-server-stream)
  (:documentation
   "A server connection that spawns streams of VANILLA-SERVER-STREAM type when a
new stream is requested, and optionally prints activities."))

(defclass vanilla-server-stream (server-stream
                                 binary-output-stream-over-data-frames
                                 body-collecting-mixin
                                 history-printing-object)
  ()
  (:documentation
   "A server-side stream that can be used as a binary output stream, optionally
prints activities, and reads full body from client if clients sends one."))

(defmethod peer-ends-http-stream ((stream vanilla-server-stream))
  "Send some random payloads, or shut down the server."
  (let ((handler
            (or
             (cdr (assoc (get-path stream)
                         (or (get-exact-handlers (get-connection stream))
                             *exact-handlers*)
                         :test (lambda (prefix path)
                                 (let ((mismatch (mismatch prefix path)))
                                   (or (null mismatch)
                                       (and (eql mismatch (position #\? path))
                                            (eql mismatch (length path))))))))
             (cdr (assoc (get-path stream)
                         (or (get-prefix-handlers (get-connection stream))
                             *prefix-handlers*)
                         :test (lambda (prefix path)
                                 (let ((mismatch (mismatch prefix path)))
                                   (or (null mismatch) (equal mismatch (length path))))))))))
    (with-slots (connection) stream
      (if handler (funcall handler connection stream)
          (progn
            (write-headers-frame stream `((:status "404") ("content-type" "text/html; charset=UTF-8")) :end-headers t)
            (with-open-stream (out (flexi-streams:make-flexi-stream stream))
              (format out  "<h1>Not found</h1>")))))))

(defun process-server-stream (stream &key (connection-class 'vanilla-server-connection))
  "Make a HTTP2 connection of CONNECTION-CLASS on the underlying STREAM (that is a
stream in Common Lisp sense, so either network stream or even standard io) and
read frames from it until END-OF-FILE (client closed the underlying stream - or
maybe we do) or GO-AWAY (client closes connection - or maybe we do) is
signalled."
  (let ((connection (make-instance connection-class
                                   :network-stream stream)))
    (with-simple-restart (close-connection "Close current connection")
      (handler-case
          (loop (read-frame connection))
        (end-of-file () nil)
        (go-away ())))))
