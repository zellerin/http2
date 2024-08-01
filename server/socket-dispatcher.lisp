(in-package http2)

;;; This is adapted from TLS-SERVER experiments package with some changes,
;;; biggest one being replacement of keywords with a class hierarchy.
(defsection @server-actions
    (:title "Generic server interface")
  "The functions below implement server creation on an abstract level. Individual
server types implement appropriate methods to ensure desired behaviour.

The main entry point is CREATE-SERVER; CALLBACK-ON-SERVER is useful for connecting to created server from separate thread.

Each dispatching method needs to implement DO-NEW-CONNECTION."
  (create-server function)
  (do-new-connection generic-function)
  (kill-server restart)
  (kill-server function)
  (kill-parent restart)
  (go-away restart)
  (callback-on-server function)
  (url-from-socket function)
  (url-from-port function)
  ;; TODO: remove/fix
  (*buffer* variable)
  (unsupported-server-setup condition))

(defun create-server (port dispatcher
                      &rest keys
                      &key
                        (host "127.0.0.1")
                        (announce-url-callback (constantly nil))
                      &allow-other-keys)
  "Create a server on HOST and PORT that handles connections using DISPATCH-METHOD.

ANNOUNCE-URL-CALLBACK is called when server is set up on the TCP level and
receives one parameter, URL that server listens on. The idea is to be able to connect
to server when PORT is 0, that is, random port, especially for automated tests.

Establishes restart KILL-SERVER to close the TCP connection and return.

Calls DO-NEW-CONNECTION to actually handle the connections after the callback
returns This function also receives the listening socket and TLS and
DISPATCH-METHOD as parameters.

Additional keyword parameters are allowed; they are defined and consumed by
individual connection methods. One of them is FULL-HTTP. Some methods use that
to use HTTP/2 library instead of simplified HTTP/2 implementation defined in
this package. The default value is intentionally unspecified."
  (when (symbolp dispatcher)
    (setf dispatcher (apply #'make-instance dispatcher :allow-other-keys t keys)))
  (restart-case
      (usocket:with-socket-listener (listening-socket host port
                                                      :reuse-address t
                                                      :element-type '(unsigned-byte 8))
        (funcall announce-url-callback (url-from-socket listening-socket host
                                                        (get-tls dispatcher)))
        (loop
          (do-new-connection listening-socket dispatcher)))
    (kill-server (&optional value) :report "Kill server" value)))

(defun url-from-socket (socket host tls)
  "Return URL that combines HOST with the port of the SOCKET.

This is to be used as callback fn on an open server for testing it."
  (make-instance 'puri:uri
                 :scheme (if tls :https :http)
                 :port (usocket:get-local-port socket)
                 :host host))

(defun url-from-port (port host tls)
  "Return URL that combines HOST with the port of the SOCKET.

This is to be used as callback fn on an open server for testing it."
  (make-instance 'puri:uri
                 :scheme (if tls :https :http)
                 :port port
                 :host host))

(define-condition unsupported-server-setup (error)
  ((dispatcher :accessor get-dispatcher :initarg :dispatcher)))

(defgeneric do-new-connection (listening-socket dispatcher)
  (:documentation
   "This method is implemented for the separate connection types. It waits on
new (possibly tls) connection to the LISTENING-SOCKET and start handling it
using DISPATCH-METHOD.

See @IMPLEMENTATIONS for available DISPATCH-METHOD.

TLS is either NIL or :TLS. Note that when using HTTP/2 without TLS, most clients
have to be instructed to use tls - e.g., --http2-prior-knowledge for curl.

Raise UNSUPPORTED-SERVER-SETUP if there is no relevant method.")
  (:method (listening-socket dispatcher)
    (error 'unsupported-server-setup :dispatcher dispatcher)))

;; 20240726 TODO: Compare with threaded-tests' call-with-test-server
;; The difference is that whether client or server is in the main thread.
(defun callback-on-server (fn &key (thread-name "Test client for a server"))
  "Return a function that takes one parameter, URL, as a parameter and calls FN on
it in a separate thread. Then it kills the server by invoking KILL-SERVER restart.

This is to be used as callback on an open server for testing it."
  (lambda (url)
    (let ((parent (bt:current-thread)))
      (bt:make-thread
       (lambda ()
         (let ((result))
           (unwind-protect
                (with-simple-restart (kill-parent "Kill parent")
                  (setf result (funcall fn url)))
             (bt:interrupt-thread parent #'kill-server result))))
       :name thread-name))))

(defun kill-server (&optional result)
  "Kill server by invoking KILL-SERVER restart, if it exists."
  (let ((restart (find-restart 'kill-server)))
    (if restart (invoke-restart restart result))))

(mgl-pax:define-restart kill-server (&optional value)
  "Restart established in CREATE-SERVER that can be invoked to terminate the server
properly and return VALUE.")

(mgl-pax:define-restart kill-parent (&optional value)
  "Restart established in CREATE-SERVER that TODO")

;;;; TODO: document and review
(mgl-pax:define-restart go-away (&optional value)
  "Handler to be invoked to close HTTP connection from our side.

It is established either in TLS-SERVER/SYNCHRONOUS:DO-CONNECTION.

TODO: Should we have it in async-cffi loop as well?")

;; TODO: why is this needed here?
(defvar *buffer* nil
  "Preallocated buffer for reading from stream. This is initialized for each
connection depending on the dispatch method.")

(defmethod get-tls ((what symbol))
  (get-tls (make-instance what)))

(defclass base-dispatcher ()
  ((tls              :reader get-tls
                     :initform nil :allocation :class)
   (connection-class :accessor get-connection-class :initarg :connection-class))
  (:default-initargs  :connection-class 'vanilla-server-connection))

(defclass single-client-dispatcher (base-dispatcher)
  ()
  (:documentation "Handle the connection while doing nothing else.

Serve just one client at time: when it connects, read the incoming requests and
handle them as they arrive. When the client sends go-away frame, close the
connection and be ready to serve another client.

Obviously, there is little overhead and this version is actually pretty fast -
for one client and in ideal conditions (especially with request pilelining)."))

(defmethod wrap-server-socket (socket dispatcher)
  (usocket:socket-stream socket))

(defmethod do-new-connection (listening-socket (dispatcher single-client-dispatcher))
  (usocket:with-connected-socket (plain (usocket:socket-accept listening-socket
                                                               :element-type '(unsigned-byte 8)))

    (process-server-stream (wrap-server-socket plain dispatcher) :connection-class (get-connection-class dispatcher))))
