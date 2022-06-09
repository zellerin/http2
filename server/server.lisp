(in-package http2)

;;;; Sample server with constant payload
(defclass sample-server-connection (server-http2-connection

                                    dispatcher-mixin
                                    ;; when *do-print-log* is set, extensive event
                                    ;; log is printed
                                    history-printing-object)
  ()
  (:default-initargs :stream-class 'sample-server-stream))

(defclass sample-server-stream (server-stream

                                body-collecting-mixin
                                ;; when *do-print-log* is set, extensive event
                                ;; log is printed
                                history-printing-object)
  ())

(defparameter *exit-on-/exit* nil
  "If T, exit when /exit request is received (on sbcl). This is used by tests to
shut down the server.")

(define-prefix-handler "/re" (redirect-handler "/ok"))

(define-exact-handler "/ok" (send-text-handler "Redirect was OK"
                                               :content-type "text/plain"
                                               :additional-headers '(("refresh" "3; url=/"))))

(defvar *intro-page*
  "<h1>Hello World</h1>
<p>This server is for testing http2 protocol</p>
<p><a href='/redir'>Redirect test</a>
<form action='/body' method='post'><input type='submit' name='xxx' value='POST query test'></form")

(define-exact-handler "/" (send-text-handler  *intro-page* ))

(define-exact-handler "/body"
    (handler
      (send-headers `((:status "200") ("content-type" "text/plain")
                      ("refresh" "3; url=/")))
      (send-text (get-body stream) :end-stream t)))

(define-exact-handler "/exit"
  (handler
    (send-headers `((:status "200")))
    (send-text "Goodbye" :end-stream t)
    (write-goaway-frame connection connection 0 +no-error+ #())
    (force-output (get-network-stream connection))
    (sb-ext:quit)))

(defmethod peer-ends-http-stream (connection (stream sample-server-stream))
  "Send some random payloads, or shut down the server."
  (let ((handler
          (or
           (cdr (assoc (get-path stream) *exact-handlers*
                          :test (lambda (prefix path)
                                  (let ((mismatch (mismatch prefix path)))
                                    (or (null mismatch)
                                        (and (eql mismatch (position #\? path))
                                             (eql mismatch (length path))))))))
           (cdr (assoc (get-path stream) *prefix-handlers*
                          :test (lambda (prefix path)
                                  (let ((mismatch (mismatch prefix path)))
                                    (or (null mismatch) (equal mismatch (length path))))))))))
    (if handler (funcall handler connection stream)
     (progn
       (write-headers-frame connection stream `((:status "404") ("content-type" "text/html")) :end-headers t)
       (write-data-frame connection stream (map 'vector 'char-code "<h1>Not found</h1>")
                         :end-stream t))))
  (force-output (get-network-stream connection)))

(defun process-server-stream (stream)
  (let ((connection (make-instance 'sample-server-connection
                                   :network-stream stream)))
    (with-simple-restart (close-connection "Close current connection")
      (handler-case
          (loop (read-frame connection))
        (end-of-file () nil)
        (go-away ())))))

#+sbcl (defun sb-threadify (fn &rest args)
         (sb-thread:make-thread fn :arguments args))

(defvar *dispatch-fn* #'funcall
  "How to call process-server-stream. Default is funcall, or sb-thredify on sbcl.")

(defun wrap-to-tls-and-process-server-stream (raw-stream key cert)
  (unwind-protect
       (handler-case
           (let ((tls-stream (cl+ssl:make-ssl-server-stream
                              raw-stream
                              :certificate cert
                              :key key)))
             (if (equal "h2" (cl+ssl:get-selected-alpn-protocol tls-stream))
                 (funcall *dispatch-fn*
                          #'process-server-stream  tls-stream)))
         (error (err)
           (describe err)))
    (close raw-stream)))

(defun make-http2-tls-context ()
  (let ((context
          (cl+ssl:make-context
           ;; Implementations of HTTP/2 MUST use TLS
           ;; version 1.2 [TLS12] or higher for HTTP/2
           ;; over TLS.
           :min-proto-version cl+ssl::+TLS1-2-VERSION+

           ;;   A deployment of HTTP/2 over TLS 1.2 MUST disable compression.  TLS
           ;;   compression can lead to the exposure of information that would not
           ;;   otherwise be revealed [RFC3749].  Generic compression is unnecessary
           ;;   since HTTP/2 provides compression features that are more aware of
           ;;   context and therefore likely to be more appropriate for use for
           ;;   performance, security, or other reasons.
           :options (list #x20000 ; +ssl-op-no-compression+
                          cl+ssl::+ssl-op-all+
                          #x40000000) ; no renegotiation
           ;; do not requiest client cert
           :verify-mode cl+ssl:+ssl-verify-none+)))
    (cl+ssl::ssl-ctx-set-alpn-select-cb
     context
     (cffi:get-callback 'cl+ssl::select-h2-callback))
    context))

(defun create-server (port key cert &key ((:verbose http2::*do-print-log*)))
  (usocket:with-server-socket (socket (usocket:socket-listen "127.0.0.1" port
                                                             :reuse-address t
                                                             :backlog 200))
    (cl+ssl:with-global-context ((make-http2-tls-context) :auto-free-p t)
      (loop
        (wrap-to-tls-and-process-server-stream
         (usocket:socket-stream
          (handler-case
              (usocket:socket-accept socket  :element-type '(unsigned-byte 8))
            ;; ignore condition
            (usocket:connection-aborted-error ())))
         key cert)))))

;;;;
;;;; curl: send settings and wait for settings.
