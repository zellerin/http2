;;;; Copyright 2022-2025 by Tomáš Zellerin

(in-package :http2/server/shared)


(defsection @server-content
    (:title "Define content for HTTP/2 server")
  "To server something else than 404 Not found, you need to define handlers for specific paths. Simple handler definition can look like

```
(define-exact-handler \"/hello-world\"
  (handler (foo :utf-8 nil)
    (with-open-stream (foo foo)
      (send-headers
       '((:status \"200\")
         (\"content-type\" \"text/html; charset=utf-8\")))
      (format foo \"Hello World, this is random: ~a\" (random 10)))))
```

This defines a handler on \"/hello-world\" path that sends reasonable headers, writes some text to the stream and closes the stream (via WITH-OPEN-STREAM). The text written is passed  to the client as data (body).

In general, the handlers are set using DEFINE-PREFIX-HANDLER or
DEFINE-EXACT-HANDLER, and are functions typically created by HANDLER macro,
or (in simple cases) by REDIRECT-HANDLER or SEND-TEXT-HANDLER functions."
  (define-prefix-handler macro)
  (define-exact-handler macro)
  (handler type)
  (handler macro)
  (constant-handler macro)
  (redirect-handler function)
  (send-text-handler function)
  (send-headers function)
  (send-goaway function))


(defsection @request-details
    (:title "Getting request details")
  "Sometimes you need to get some data from the request.

These data can be carried by querying the HTTP/2 stream object involved. If you
define handlers by HANDLER macro, it is available in a lexically bound \\STREAM
variable.

```
(define-exact-handler \"/body\"
  (handler (foo :utf-8 nil)
    (with-open-stream (foo foo)
      (send-headers
       '((:status \"200\")
         (\"content-type\" \"text; charset=utf-8\")))
      (format foo \"Hello World, this is a ~s request.~3%content~%~s~3%headers~%~s~%~3%body~%~s~%\"
         (http2/core::get-method stream)
         (http-stream-to-string stream)
         (http2/core::get-headers stream)
         (http2/core::get-body stream)))))
```

"
  (get-path generic-function)
  (get-headers (method (HTTP2/core::header-collecting-mixin)))
  (get-method (method (server-stream)))
  (get-scheme (method (server-stream)))
  (get-authority (method (server-stream)))
  (@request-body section))

(defsection @request-body
    (:title "Body of the request")
  "Sometimes there is a body in the client request.

When sending a such a request, you can use CONTENT parameter of the
HTTP2/CLIENT:RETRIEVE-URL, together with CONTENT-TYPE.

```
 (http2/client:retrieve-url \"https://localhost:8080/body\" :content \"Hello\")
 (http2/client:retrieve-url \"https://localhost:8080/body\"
       :content #(1 2 3) :content-type \"application/octet-stream\")
```

When you write a handler for such a request, you should know if you want binary
or text data. The vanilla class for the server streams looks at the headers, and
if they look like UTF-8 (as per IS-UTF8-P), it processes the data as text, if
not, they are collected as binary vector.

When your client systematically send headers that do not make it TEXT and you
want to read text, as last resort change class of your streams to include
FALLBACK-ALL-IS-ASCII (or improve IS-UTF8-P, or add some other decoding function).

If you do not want to see text at all, change class to \\NOT include
UTF8-PARSER-MIXIN or any other conversion mixin."
  (get-body (method (body-collecting-mixin)))
  (http-stream-to-string function)
  (http2/client::fallback-all-is-ascii class))

(defun send-goaway (code debug-data)
  "Start closing connection, sending CODE and DEBUG-DATA in the go-away frame to
peer. Must be called from inside of HANDLER macro."
  (declare (ignore code debug-data))
  (error "SEND-GOAWAY must be used inside HANDLER macro."))

(defclass routing-mixin ()
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
"))

(eval-when (:compile-toplevel :load-toplevel)
  (defun define-some-handler (target prefix fn)
    `(setf ,target
           (acons ,prefix ,fn
                  (remove ,prefix ,target :key 'car :test 'equal)))))

(deftype handler ()
  "Function that can be called with CONNECTION and HTTP2-STREAM to write a response to
the http request described by STREAM object."
  `(function (connection http2-stream)))

(defmacro handler ((flexi-stream-name charset gzip) &body body)
  "Return a HANDLER type function.

 This handler, when called, runs BODY in a context where

- FLEXI-STREAM-NAME is bound to an open flexi stream that can be written to (to write response). On background, written text is converted from CHARSET to octets, possibly compressed by GZIP and split into frames,
- and two lexical functions are defined, SEND-HEADERS and SEND-GOAWAY.

The SEND-HEADERS sends the provided headers to the STREAM.

The SEND-GOAWAY sends go away frame to the client to close connection.

The handler body needs to close the underlying stream if the response is
actually to be sent, or possibly schedule sending more data for later."
  `(lambda (connection stream)
     (let ((,flexi-stream-name
             (make-transport-output-stream stream ,charset ,gzip)))
       (flet ((send-headers (&rest args)
                (apply #'send-headers stream args))
              (send-goaway (code debug-data)
                (write-goaway-frame connection
                                    0 code debug-data)
                (flush-http2-data connection)))
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
                (flush-http2-data connection))
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

TEXT is evaluated when handler is defined, not when handler is invoked. For
content that can change on individual invocations write to the stream.

ADDITIONAL-HEADERS are sent along with :status and content-type
headers."
  (constant-handler (out :utf-8 gzip
                     `((:status "200") ("content-type" ,content-type)
                       ,@additional-headers))
    (princ text out)))

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
                                     routing-mixin
                                     threaded-server-mixin
                                     stream-based-connection-mixin)
  ()
  (:default-initargs :stream-class 'vanilla-server-stream)
  (:documentation
   "A server connection that spawns streams of VANILLA-SERVER-STREAM type when a
new stream is requested, allows scheduled or other asynchronous writes, and
optionally prints activities."))

(defclass vanilla-server-stream (server-stream
                                 utf8-parser-mixin fallback-all-is-ascii  text-collecting-stream
                                 http2/core::header-collecting-mixin
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
  (log-closed-stream stream "- processing")
  (let ((connection (http2/core::get-connection stream)))
    (funcall (find-matching-handler (get-path stream) connection) connection stream)))

(defgeneric cleanup-connection (connection)
  (:method (connection) nil)
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
  (let ((connection (or connection (make-instance connection-class))))
    (restart-case
        (handler-case
            (unwind-protect
                 (progn
                   (setf (get-network-stream connection) stream)
                   (log-server-connected connection)
                 (process-pending-frames connection nil #'parse-client-preface (length +client-preface-start+))))
          (end-of-file (e) (invoke-restart 'close-connection e)))
      (close-connection (error)
        :report "Cleanly close current connection"
        :interactive (lambda () (list "Interactive restart"))
        (log-server-disconnected connection error)
        (cleanup-connection connection)))))

(defsection @dispatchers
    ()
  (detached-tls-single-client-dispatcher class)
  (detached-single-client-dispatcher class)
  (detached-tls-threaded-dispatcher class))

(defclass tls-single-client-dispatcher (tls-dispatcher-mixin single-client-dispatcher)
  ())

(defclass detached-tls-single-client-dispatcher (detached-server-mixin tls-single-client-dispatcher)
  ())


(defsection @server-start-stop (:title "Starting HTTP/2 server")
  "Start server on foreground with RUN, or on background with START. You can stop server on background with STOP.

This creates (as of this version) a multithreaded server that serves 404 Not
found responses on any request.

```
(http2/server:start 8443)
==> #<HTTP2/SERVER:DETACHED-TLS-THREADED-DISPATCHER HTTP/2 server on https://localhost:8443/>
==> #<PURI:URI https://localhost:8443/>
;; run curl -k https://localhost:8443/
127.0.0.1:58565 Connected, using VANILLA-SERVER-CONNECTION
127.0.0.1:58565 / [#1] - processing
```
")

(defsection @server-reference (:title "Server reference")
  (start function)
  (run function)
  (stop function))

(defvar *servers* nil
  "List of started servers")

(defun start (port &rest args &key
                                (host *vanilla-host*)
                                (dispatcher *vanilla-server-dispatcher*)
              &allow-other-keys)
  "Start a default HTTP/2 https server on PORT on background.

Returns two values with a detached (see below) dispatcher, which is default:

- Server instance (that is appropriate parameter for stop)
- base url of the server (most useful when PORT was 0 - any free port)

With a non-detached dispatcher the value is not specified.

DISPATCHER parameter sets the dispatcher to use for the server. Dispatchers
determine how are new requests handled. Presently there are several sets of
dispatchers defined, see @DISPATCHERS

HOST is used to bind appropriately the listening socket. Typically it would be
localhost or \"0.0.0.0\".

Detached variants run the server in a separate thread and returns immediately
after opening the socket.

Value of *VANILLA-SERVER-DISPATCHER* is not specified (set it if you care) but
should be presently best detached dispatcher.

PRIVATE-KEY-FILE and CERTIFICATE-FILE determine where to look for the
certificate and the private key. By default, HOSTNAME (defaulting to HOST) is
used to find the file in predefined locations (see FIND-PRIVATE-KEY-FILE)
respective parameters try to locate the files."
  (declare (optimize debug safety (speed 0)))
  (multiple-value-bind (server socket)
      (apply #'create-server port dispatcher
             :host host
             args)
    (push server *servers*)
    (values server (url-from-socket socket host t))))

(defun run (port &rest pars &key certificate-file private-key-file)
  "Run a default HTTP/2 server on PORT on foreground.

The dispatcher type is not defined and can change in future. This is a thin
wrapper over START that cannot be customized."
  (declare (ignore certificate-file private-key-file))
  (apply 'start port :dispatcher 'tls-threaded-dispatcher pars))

(defun stop (&optional (server (car *servers*)))
  "Stop a server and remove it from list of servers."
  (cond
    ((null server)
     (warn "No running server to stop"))
    (t
     (restart-case (stop-server server)
       (continue ()
         :report "Just remove server from list of running servers"
         :test (lambda (err) #+sbcl(typep err 'SB-THREAD:INTERRUPT-THREAD-ERROR)
                       #-sbcl t)))
     (setf *servers* (remove server *servers*))))
  server)

(defmethod stop-server ((dispatcher detached-server-mixin))
  ;; TODO: does it work for non-sbcl lisps? As in "implementation-defined" note
  ;; in the doc of destroy-thread.
  (bordeaux-threads:destroy-thread  (get-thread dispatcher)))


(defsection @server-reference
    (:title "Server API reference")
  (@dispatchers section))

(defsection @dispatchers
    (:title "Server classes")
  "The server behaviour is defined by used dispatcher (server class). The class is
specified as a parameter to the START function."
  "The dispatcher should be a subclass of the BASE-DISPATCHER and can use predefined mixins to specify:

- How to handle multiple connections (THREADED-DISPATCHER, POLL-DISPATCHER-MIXIN, SINGLE-CLIENT-DISPATCHER)
- For threaded dispatcher to add TLS wrapping (TLS-DISPATCHER-MIXIN)
- To run the server in separate thread (DETACHED-SERVER-MIXIN).

Some predefined combinations are below."
  (poll-dispatcher class)
  (detached-poll-dispatcher class)
  (detached-tls-threaded-dispatcher class)
  (tls-threaded-dispatcher class)
  (tls-single-client-dispatcher class)
  (detached-tls-single-client-dispatcher class)
  (*vanilla-server-dispatcher* (variable nil))
  (@server-mixins section))

(defsection @server-mixins
    (:title "Server mixins and component classes")
  (base-dispatcher class)
  (threaded-dispatcher class)
  (poll-dispatcher-mixin class)
  (single-client-dispatcher class)
  (tls-dispatcher-mixin class)
  (detached-server-mixin class))

"-  use polling interface that uses openssl directly,
- TLS-THREADED-DISPATCHER and DETACHED-TLS-THREADED-DISPATCHER use thread per connection and use CL+SSL,
- TLS-SINGLE-CLIENT-DISPATCHER and DETACHED-TLS-SINGLE-CLIENT-DISPATCHER handle one connection at time
  in a single thread using CL+SSL and is simplest of these.

There are also non-TLS variants of the -TLS- dispatchers to simplify finding errors."

;;;; TLS dispatcher
(defclass tls-dispatcher-mixin (certificate-h2-dispatcher)
  ()
  (:documentation
   "Specializes SERVER-SOCKET-STREAM to add TLS layer to the created sockets,
and START-SERVER-ON-SOCKET to use a context created by MAKE-HTTP2-TLS-CONTEXT."))

(defmethod get-tls  ((dispatcher tls-dispatcher-mixin)) t)
