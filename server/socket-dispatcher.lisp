(in-package #:http2/server/shared)

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
  (unsupported-server-setup condition)
  (server-socket-stream generic-function)
  (start-server-on-socket generic-function)
  (find-private-key-file function)
  (find-certificate-file function)
  (maybe-create-certificate function))

(defclass detached-server-mixin ()
  ((thread :accessor get-thread :initarg :thread)
   (name   :accessor get-name   :initarg :name))
  (:default-initargs :name "HTTP/2 server"))

(defmethod print-object ((server detached-server-mixin) out)
  (print-unreadable-object (server out :type t)
    (with-slots (name url) server
      (format out "~a~@[ on ~a~]" name url))))

(defgeneric start-server-on-socket (dispatcher socket)
  (:method (dispatcher listening-socket)
    (usocket:with-server-socket (socket listening-socket)
      (loop (do-new-connection listening-socket dispatcher))))
  (:method ((dispatcher detached-server-mixin) socket)
    (with-slots (thread name) dispatcher
      (setf (get-url dispatcher) (url-from-socket socket *vanilla-host* (get-tls dispatcher))
            thread (bordeaux-threads:make-thread
                    #'call-next-method
                    :name name)))
    (values dispatcher socket)))

(defvar *vanilla-server-dispatcher* 'detached-tls-threaded-dispatcher
  "Default value of the server dispatcher. One of DETACHED-TLS-THREADED-DISPATCHER
ot POLL-DISPATCHER")
(defvar *vanilla-host* "localhost")

(defun find-private-key-file (hostname)
  "Find the private key for HOSTNAME or create it.

Look for
- /etc/letsencrypt/live/<hostname>privkey.pem (this is where let's encrypt stores them)
- file named <hostname>.key in /tmp (ad-hoc generated files)

If it does not exist, generate the key and self signed cert in /tmp/"
  (let* ((key-name (make-pathname :name hostname :defaults "/tmp/foo.key"))
         (cert-name (make-pathname :type "crt" :defaults key-name))
         (lets-encrypt-name
           (make-pathname :directory `(:absolute "etc" "letsencrypt" "live" ,hostname)
                          :name "privkey"
                          :type "pem")))
    (cond ((probe-file key-name))
          ((probe-file lets-encrypt-name) lets-encrypt-name) ; explicit needed, symlinks
          (t
           (warn "No private key found by heuristics, creating new pair in /tmp")
           (maybe-create-certificate key-name cert-name :base "/tmp")))))

(defun find-certificate-file (keypath)
  "Find a certificate file for private key stored in KEYPATH.

Try file of same name ending with .crt, or, if the name of private key was privkey.pem, try fullchain.pem (this is what let's encrypt uses)."
  (or
   (probe-file (make-pathname :type "crt" :defaults keypath))
   (and (equal (pathname-name keypath) "privkey")
        (probe-file (make-pathname :name "fullchain" :defaults keypath)))
   (error "Cannot find cert file")))

(defun create-server (port dispatcher
                      &rest keys
                      &key
                        (host "127.0.0.1")
                      &allow-other-keys)
  "Create a server on HOST and PORT that handles connections using DISPATCH-METHOD.

Establishes restart KILL-SERVER to close the TCP connection and return.

Calls DO-NEW-CONNECTION to actually handle the connections after the callback
returns This function also receives the listening socket and TLS and
DISPATCH-METHOD as parameters.

Additional keyword parameters are allowed; they are defined and consumed by
the dispatcher."
  (when (symbolp dispatcher)
    (setf dispatcher (apply #'make-instance dispatcher :allow-other-keys t keys)))
  (restart-case
      (let ((listening-socket (usocket:socket-listen host port
                                                     :reuse-address t
                                                     :element-type '(unsigned-byte 8))))
        (start-server-on-socket dispatcher listening-socket))
    (kill-server (&optional value) :report "Kill server"
      (setf (get-url dispatcher) nil)
      value)))

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
  ((tls              :reader   get-tls
                     :initform nil               :allocation :class)
   (connection-class :accessor get-connection-class :initarg  :connection-class)
   (connection-args  :accessor get-connection-args  :initarg  :connection-args)
   (url              :accessor get-url              :initarg  :url))
  (:default-initargs
   :connection-class 'vanilla-server-connection
   :connection-args nil))

(defclass single-client-dispatcher (base-dispatcher)
  ()
  (:documentation "Handle the connection while doing nothing else.

Serve just one client at time: when it connects, read the incoming requests and
handle them as they arrive. When the client sends go-away frame, close the
connection and be ready to serve another client.

Obviously, there is little overhead and this version is actually pretty fast -
for one client and in ideal conditions (especially with request pilelining)."))


(defgeneric server-socket-stream (socket dispatcher)
  (:method (socket dispatcher)
    (usocket:socket-stream socket))
  (:documentation
   "Make a Lisp stream from a socket. This is primarily used as a hook to insert TLS
layer when needed."))

(defmacro with-standard-handlers (() &body body)
  `(handler-bind
       ((CL+SSL::SSL-ERROR-SSL 'abort)
        (CL+SSL::SSL-ERROR-SYSCALL 'abort)
        (stream-error 'abort)
        (usocket:connection-aborted-error 'abort))
     ,@body))

(defmethod do-new-connection (listening-socket (dispatcher single-client-dispatcher))
  (with-standard-handlers ()
    (usocket:with-connected-socket (plain (usocket:socket-accept listening-socket
                                                                 :element-type '(unsigned-byte 8)))
      (process-server-stream (server-socket-stream plain dispatcher)
                             :connection (apply #'make-instance (get-connection-class dispatcher)
                                                (get-connection-args dispatcher))))))

(defun maybe-create-certificate (key certificate &key system (base
                                                              (if system (asdf:component-pathname (asdf:find-system system)) #P"/tmp/")))
  "Generate key and a self-signed certificate to it for localhost using openssl
cli."
  (unless (and (probe-file key)
               (probe-file certificate))
    (let ((key-file (ensure-directories-exist (merge-pathnames key base)))
          (cert-file (ensure-directories-exist (merge-pathnames certificate base))))
      (uiop:run-program
       `("openssl" "req" "-new" "-nodes" "-x509" "-days" "365" "-subj" "/CN=localhost" "-keyout" ,(namestring key-file)
                   "-outform" "PEM" "-out" ,(namestring cert-file)))
      (terpri)
      (values key-file cert-file))))
