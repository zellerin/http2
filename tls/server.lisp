;;;; Copyright 2022, 2023 by Tomáš Zellerin

(in-package http2)

(defsection @server/threaded
    (:title "Threaded server")
  (threaded-dispatch function))

(defun threaded-dispatch (fn tls-stream &rest pars)
  "Apply FN on the TLS-STREAM and PARS in a new thread

To be used as *DISPATCH-FN* callback for thread-per-connection request handling."
  (bt:make-thread (lambda ()
                    (apply fn tls-stream pars))
                  :name "HTTP/2 connection handler"))

(defvar *dispatch-fn* #'threaded-dispatch
  "How to call process-server-stream. Default is THREADED-DISPATCH.

The function is called with PROCESS-SERVER-STREAM as the first parameter and its
parameters following.")

(define-condition not-http2-stream (serious-condition)
  ((tls-stream :accessor get-tls-stream :initarg :tls-stream)
   (alpn       :accessor get-alpn       :initarg :alpn))
  (:documentation
   "Signalled to decline handling of TLS stream as HTTP2 stream due to different ALPN.")
  (:report (lambda (condition stream)
             (format stream "The TLS stream ~A is not a HTTP2 stream (ALPN ~s)"
                     (get-tls-stream condition)
                     (get-alpn condition)))))

(defun wrap-to-tls-and-process-server-stream (raw-stream key cert &rest args)
  "Establish TLS connection over RAW-STREAM, and run PROCESS-SERVER-STREAM over it.

Use TLS KEY and CERT for server identity.

ARGS are passed to PROCESS-SERVER-STREAM that is invoked using *DISPATCH-FN* to
allow threading, pooling etc.

Wrap call with an error handler for tls-level errors.

Raise error when H2 is not the selected ALPN protocol. In this case, the TLS
stream is kept open and caller is supposed to either use it or close it."
  (let ((tls-stream
          (handler-case
              (cl+ssl:make-ssl-server-stream
               raw-stream
               :certificate cert
               :key key)
            (error (e)
              (print e)
              (close raw-stream)
              (return-from wrap-to-tls-and-process-server-stream)))))
    (cond ((equal "h2" (cl+ssl:get-selected-alpn-protocol tls-stream))
           (apply *dispatch-fn*
                  (lambda (tls-stream &rest args)
                    (handler-case
                        (unwind-protect
                             (apply #'process-server-stream tls-stream args)
                          ;; TODO: do closing properly. This is sometimes second
                          ;; close and fails, why?
                          (ignore-errors (close tls-stream)))
                      (cl+ssl::ssl-error-ssl ()
                        ;; Just ignore it for now, Maybe it should be
                        ;; logged if not trivial - but what is trivial and
                        ;; what is to log?
                        )))
                  tls-stream args))
          (t (warn 'not-http2-stream
                    :tls-stream tls-stream
                    :alpn (cl+ssl:get-selected-alpn-protocol tls-stream))
             (close tls-stream)))))

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
- Do not allow ssl compression adn renegotiation.
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
           ;; do not requiest client cert
           :verify-mode cl+ssl:+ssl-verify-none+)))
    (let ((topdir (asdf:component-pathname (asdf:find-system "tls-server"))))
      (ssl-ctx-use-certificate-chain-file context (namestring (merge-pathnames "certs/server.pem" topdir)))
      (ssl-ctx-use-private-key-file context (namestring (merge-pathnames "certs/server.key" topdir)) cl+ssl::+ssl-filetype-pem+))
    (cl+ssl::ssl-ctx-set-alpn-select-cb
     context
     (cffi:get-callback 'cl+ssl::select-h2-callback))
    context))

(defun create-https-server (port key cert &key
                                            (announce-open-fn (constantly nil))
                                            (connection-class 'vanilla-server-connection)
                                            connection
                                            (host "127.0.0.1"))
  "Open TLS wrapped HTTPS(/2) server on PORT on HOST (localhost by default).

It accepts new connections and uses WRAP-TO-TLS-AND-PROCESS-SERVER-STREAM to
establish TLS.

ANNOUNCE-OPEN-FN is called, when set, to inform caller that the server is up and
running. This is used for testing, when we need to have the server running (in a
thread) to start testing it."
  (restart-case
    (usocket:with-server-socket (socket (usocket:socket-listen host port
                                                               :reuse-address t
                                                               :backlog 200
                                                               :element-type '(unsigned-byte 8)))
      (cl+ssl:with-global-context ((make-http2-tls-context) :auto-free-p t)
        (funcall announce-open-fn socket)
        (loop
          (wrap-to-tls-and-process-server-stream
           (usocket:socket-stream
            (handler-case
                (usocket:socket-accept socket :element-type '(unsigned-byte 8))
              ;; ignore condition
              (usocket:connection-aborted-error ())))
           key cert :connection-class connection-class :connection connection))))
    (kill-server (&optional value)
      :report "Kill server"
      value)))

(defvar *http2-tls-context* (make-http2-tls-context)
  "TLS context to use for our servers.")

(defun wrap-to-tls (raw-stream)
  "Return a binary stream representing TLS server connection over RAW-STREAM.

Use TLS KEY and CERT for server identity, and *HTTP2-TLS-CONTEXT* for the contex.

This is a simple wrapper over CL+SSL."
  (cl+ssl:with-global-context (*http2-tls-context* :auto-free-p nil)
    (let* ((topdir (asdf:component-pathname (asdf:find-system "tls-server")))
           (tls-stream
             (cl+ssl:make-ssl-server-stream
              (usocket:socket-stream raw-stream)
              :certificate
              (namestring (merge-pathnames #P"certs/server.pem" topdir))
              :key (namestring (merge-pathnames #P"certs/server.key" topdir)))))
      tls-stream)))

(defclass tls-dispatcher-mixin ()
  ((tls :reader get-tls :initform :tls
        :allocation :class)))

(defmethod wrap-server-socket (socket (dispatcher tls-dispatcher-mixin))
  (wrap-to-tls socket))

(defclass tls-single-client-dispatcher (tls-dispatcher-mixin single-client-dispatcher)
  ())

(defclass threaded-dispatcher (base-dispatcher)
  ())

(defmethod do-new-connection (listening-socket (dispatcher threaded-dispatcher))
  (let ((socket (usocket:socket-accept listening-socket
                                       :element-type '(unsigned-byte 8))))
    (bt:make-thread
     (lambda ()
       (unwind-protect
            (process-server-stream (wrap-server-socket socket dispatcher)
                                   :connection-class (get-connection-class dispatcher))
         (usocket:socket-close socket)))
     ;; TODO: peer IP and port to name?
     :name "HTTP2 server thread for connection" )))

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
