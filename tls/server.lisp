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
(defclass tls-dispatcher-mixin (certificated-dispatcher)
  ((tls              :reader   get-tls              :initform :tls
                     :allocation :class))
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
  (cl+ssl:with-global-context ((make-http2-tls-context server) :auto-free-p t)
    (call-next-method)))
