;;;; Copyright 2022-2024 by Tomáš Zellerin

(in-package http2)

(defsection @server/threaded
    (:title "Threaded server")
  (make-http2-tls-context function)
  (create-https-server function)
  (@server-classes section)
  (@server-actions section))

(define-condition not-http2-stream (serious-condition)
  ((tls-stream :accessor get-tls-stream :initarg :tls-stream)
   (alpn       :accessor get-alpn       :initarg :alpn))
  (:documentation
   "Signalled to decline handling of TLS stream as HTTP2 stream due to different ALPN.")
  (:report (lambda (condition stream)
             (format stream "The TLS stream ~A is not a HTTP2 stream (ALPN ~s)"
                     (get-tls-stream condition)
                     (get-alpn condition)))))

(cl+ssl::define-ssl-function ("SSL_CTX_use_certificate_chain_file" ssl-ctx-use-certificate-chain-file)
  :int
  (ctx cl+ssl::ssl-ctx)
  (filename :string))

(cl+ssl::define-ssl-function ("SSL_CTX_use_PrivateKey_file" ssl-ctx-use-private-key-file)
  :int
  (ctx cl+ssl::ssl-ctx)
  (filename :string)
  (type :int))

(defun make-http2-tls-context ()
  "Make TLS context suitable for http2.

Practically, it means:
- ALPN callback that selects h2 if present,
- Do not request client certificates
- Do not allow ssl compression and renegotiation.
We should also limit allowed ciphers, but we do not."
  (let ((context
          (cl+ssl:make-context
           ;; Implementations of HTTP/2 MUST use TLS
           ;; version 1.2 [TLS12] or higher for HTTP/2
           ;; over TLS.
           :min-proto-version cl+ssl::+TLS1-2-VERSION+

           :options (list #x20000 ; +ssl-op-no-compression+
                          cl+ssl::+ssl-op-all+
                          #x40000000) ; no renegotiation
           ;; do not request client cert
           :verify-mode cl+ssl:+ssl-verify-none+)))
    (let ((topdir (asdf:component-pathname (asdf:find-system "http2"))))
      (ssl-ctx-use-certificate-chain-file context (namestring (merge-pathnames "certs/server.crt" topdir)))
      (ssl-ctx-use-private-key-file context (namestring (merge-pathnames "certs/server.key" topdir)) cl+ssl::+ssl-filetype-pem+))
    (cl+ssl::ssl-ctx-set-alpn-select-cb
     context
     (cffi:get-callback 'cl+ssl::select-h2-callback))
    (let ((address (cffi:pointer-address context)))
      ;; make sure it is deallocated when no longer needed
      ;; 20240827 TODO: What happens on save-and-die?
      (trivial-garbage:finalize context
                                (lambda () (cl+ssl:ssl-ctx-free (cffi:make-pointer address)))))))

(defvar *h2-tls-context*)

(defun create-https-server (port key cert &key
                                            (connection-class 'vanilla-server-connection)
                                            (host "127.0.0.1"))
  "Open TLS wrapped HTTPS(/2) server on PORT on HOST (localhost by default).

It accepts new connections and uses WRAP-TO-TLS-AND-PROCESS-SERVER-STREAM to
establish TLS.

ANNOUNCE-OPEN-FN is called, when set, to inform caller that the server is up and
running. This is used for testing, when we need to have the server running (in a
thread) to start testing it."
  (create-server port 'tls-single-client-dispatcher :key key :certificate cert
                                                    :host host
                                                    :connection-class connection-class))


;;;; TLS dispatcher
(defclass tls-dispatcher-mixin ()
  ((tls              :reader   get-tls              :initform :tls
                     :allocation :class)
   (certificate-file :accessor get-certificate-file :initarg  :certificate-file)
   (private-key-file :accessor get-private-key-file :initarg  :private-key-file))
  (:documentation "Adds TLS layer to the created sockets."))

(defmethod server-socket-stream (socket (dispatcher tls-dispatcher-mixin))
  "The cl-ssl server socket."
  (with-slots (certificate-file private-key-file) dispatcher
    (cl+ssl:with-global-context (*h2-tls-context* :auto-free-p nil)
          (cl+ssl:make-ssl-server-stream
     (call-next-method)
     :certificate certificate-file
     :key private-key-file))))

(defmethod start-server-on-socket ((dispatcher tls-dispatcher-mixin) socket)
  "For a TLS server wrap the global context."
  (let ((*h2-tls-context* (make-http2-tls-context)))
    (unwind-protect (call-next-method)
      (when *h2-tls-context*
        (cl+ssl:ssl-ctx-free *h2-tls-context*)
        (setf *h2-tls-context* nil)))))

(defclass tls-single-client-dispatcher (tls-dispatcher-mixin detached-server-mixin single-client-dispatcher)
  ())

(defclass threaded-dispatcher (base-dispatcher)
  ())

(defclass tls-threaded-dispatcher (threaded-dispatcher tls-dispatcher-mixin detached-server-mixin)
  ())

(defmethod do-new-connection (listening-socket (dispatcher threaded-dispatcher))
  (let ((socket (usocket:socket-accept listening-socket
                                       :element-type '(unsigned-byte 8))))
    (bt:make-thread
     (lambda ()
       (with-open-stream (stream (server-socket-stream socket dispatcher))
         (process-server-stream stream
                                :connection-class (get-connection-class dispatcher))))
     ;; TODO: peer IP and port to name?
     :name "HTTP2 server thread for connection" )))

#+obsolete
(defun create-http-server (port &key
                                  (announce-open-fn (constantly nil))
                                  (connection-class 'vanilla-server-connection))
  "Open HTTP/2 server on PORT on localhost (no TLS).

Callbacks defined as methods for the CONNECTION-CLASS are used to implement
behaviour of the server.

ANNOUNCE-OPEN-FN is called, when set, to inform caller that the server is up and
running. This is used for testing, when we need to have the server running (in a
thread) to start testing it."
  (usocket:with-server-socket (socket (usocket:socket-listen "127.0.0.1" port
                                                             :reuse-address t
                                                             :backlog 200
                                                             :element-type '(unsigned-byte 8)))
    (funcall announce-open-fn socket)
    (loop
      (funcall *dispatch-fn*
               #'process-server-stream
               #+nil(lambda (tls-stream &rest args)
                      (handler-case
                          (unwind-protect
                               (apply #'process-server-stream)
                            (close tls-stream))
                        (cl+ssl::ssl-error-ssl ()
                          ;; Just ignore it for now, Maybe it should be
                          ;; logged if not trivial - but what is trivial and
                          ;; what is to log?
                          )))
               (usocket:socket-stream
                (handler-case
                    (usocket:socket-accept socket :element-type '(unsigned-byte 8))
                  ;; ignore condition
                  (usocket:connection-aborted-error ())))
               :connection-class connection-class))))
