;;;; Copyright 2022, 2023 by Tomáš Zellerin

(in-package http2)

(defun threaded-dispatch (fn tls-stream &rest pars)
  "Apply FN on the TLS-STREAM and PARS in a new thread and return true value.

To be used as *dispatch-fn* callback for thread-per-connection request handling."
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
                          (close tls-stream))
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

(defun create-http-server (port &key
                                  (announce-open-fn (constantly nil))
                                  (connection-class 'vanilla-server-connection))
  "Open HTTP/2 server on PORT on localhost (no TLS).

Callbacks defined as methods for the CONNECTION-CLASS are used to implement
behaviour of the server.

ANNOUNCE-OPEN-FN is called, when set, to inform caller that the server is up and
running. This is used for testing, when we need to have the server running (in a
thread) to start testing it.

If VERBOSE is set and CONNECTION-CLASS is derived from LOGGING-CLASS, verbose
debug is printed."
  (usocket:with-server-socket (socket (usocket:socket-listen "127.0.0.1" port
                                                             :reuse-address t
                                                             :backlog 200
                                                             :element-type '(unsigned-byte 8)))
    (funcall announce-open-fn)
    (loop
      (process-server-stream (usocket:socket-stream
                              (handler-case
                                  (usocket:socket-accept socket :element-type '(unsigned-byte 8))
                                ;; ignore condition
                                (usocket:connection-aborted-error ())))
                             :connection-class connection-class))))
