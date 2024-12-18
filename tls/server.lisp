;;;; Copyright 2022-2024 by Tomáš Zellerin

(in-package http2/cl+ssl)

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
    (cl+ssl::ssl-ctx-set-alpn-select-cb context (cffi:get-callback 'cl+ssl::select-h2-callback))
    context))

#+nil(defun create-https-server (port key cert &key
                                            (connection-class 'tls-single-client-dispatcher)
                                            (host "127.0.0.1"))
  "Open TLS wrapped HTTPS(/2) server on PORT on HOST (localhost by default).

It accepts new connections and uses WRAP-TO-TLS-AND-PROCESS-SERVER-STREAM to
establish TLS.

User provided KEY and CERT-ificate.

It is actually a wrapper over CREATE-SERVER with a default server
class (TLS-SINGLE-CLIENT-DISPATCHER)."
  (create-server port connection-class :certificate-file cert
                 :private-key-file key))

(defsection @server-classes
    (:title "Server classes")
  (tls-dispatcher-mixin class)
  (single-client-dispatcher class)
  (tls-single-client-dispatcher class)
  (threaded-dispatcher class)
  (tls-threaded-dispatcher class))

;;;; TLS dispatcher
(defclass tls-dispatcher-mixin ()
  ((tls              :reader   get-tls              :initform :tls
                     :allocation :class)
   (certificate-file :accessor get-certificate-file :initarg  :certificate-file)
   (private-key-file :accessor get-private-key-file :initarg  :private-key-file))
  (:documentation
   "Specializes SERVER-SOCKET-STREAM to add TLS layer to the created sockets,
and START-SERVER-ON-SOCKET to use a context created by MAKE-HTTP2-TLS-CONTEXT."))

(defmethod http2/server::server-socket-stream (socket (dispatcher tls-dispatcher-mixin))
  "The cl-ssl server socket."
  (with-slots (certificate-file private-key-file) dispatcher
    (cl+ssl:make-ssl-server-stream
     (call-next-method)
     :certificate certificate-file
     :key private-key-file)))

"For a TLS server wrap the global context."
(defmethod http2/server::start-server-on-socket ((server tls-dispatcher-mixin) socket)
  (cl+ssl:with-global-context ((make-http2-tls-context) :auto-free-p t)
    (call-next-method)))
