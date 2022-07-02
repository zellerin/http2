(in-package http2)

(defvar *dispatch-fn* #'funcall
  "How to call process-server-stream. Default is funcall.

The function is called with PROCESS-SERVER-STREAM as the first parameter and its
parameters following.")

(defun wrap-to-tls-and-process-server-stream (raw-stream key cert &rest args)
  "Establish TLS connection over RAW-STREAM, and run PROCESS-SERVER-STREAM over it.

Use TLS KEY and CERT for server identity.

ARGS are passed to PROCESS-SERVER-STREAM that is invoked using *DISPATCH-FN* to
allow threading, pooling etc.

Wrap call to  with an error handler.

Raise error when H2 is not the selected ALPN protocol."
  (unwind-protect
       (handler-case
           (let ((tls-stream (cl+ssl:make-ssl-server-stream
                              raw-stream
                              :certificate cert
                              :key key)))
             (if (equal "h2" (cl+ssl:get-selected-alpn-protocol tls-stream))
                 (apply *dispatch-fn*
                        #'process-server-stream  tls-stream args)
                 (error "Someone else would need to handle non-h2 queries (~a)~%"
                       (cl+ssl:get-selected-alpn-protocol tls-stream))))
         (error (err)
           (describe err)))
    (close raw-stream)))

(defun make-http2-tls-context ()
  "Make TLS context suitable for http2.

Practically, it means:
- ALPN callback that selects h2 if present,
- Do not request client certificates
- Do not allow ssl compression adn renegotiation.

``A deployment of HTTP/2 over TLS 1.2 MUST disable compression.  TLS compression
can lead to the exposure of information that would not otherwise be revealed
[RFC3749].  Generic compression is unnecessary since HTTP/2 provides compression
features that are more aware of context and therefore likely to be more
appropriate for use for performance, security, or other reasons.''

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
                                            ((:verbose http2::*do-print-log*))
                                            (announce-open-fn (constantly nil))
                                            (connection-class 'vanilla-server-connection))
  "Open TLS wrapped HTTPS(/2) server on PORT on localhost.

It accepts new connections and uses WRAP-TO-TLS-AND-PROCESS-SERVER-STREAM to
establish TLS.

callbacks defined as methods for the CONNECTION-CLASS are used to implement
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
    (cl+ssl:with-global-context ((make-http2-tls-context) :auto-free-p t)
      (funcall announce-open-fn)
      (loop
        (wrap-to-tls-and-process-server-stream
         (usocket:socket-stream
          (handler-case
              (usocket:socket-accept socket :element-type '(unsigned-byte 8))
            ;; ignore condition
            (usocket:connection-aborted-error ())))
         key cert :connection-class connection-class)))))
