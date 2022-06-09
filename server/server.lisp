(in-package http2)

;;;; Sample server with constant payload
(defclass sample-server-connection (server-http2-connection history-printing-object)
  ()
  (:default-initargs :stream-class 'sample-server-stream))

(defclass sample-server-stream (http2-stream history-printing-object)
  ((path    :accessor get-path    :initarg :path))
  (:default-initargs :path :undefined))

(defmethod print-object ((stream sample-server-stream) out)
  (print-unreadable-object (stream out :type t)
    (format out "#~d ~s ~s" (get-stream-id stream) (get-path stream) (get-state stream))))

(defmethod add-header (connection (stream sample-server-stream) (name (eql :path)) value)
  (setf (get-path stream) value))

(defparameter *text-to-send* (format nil "<h1>Hello World</h1>~&"))
(defparameter *headers-to-send* '(("content-type" "text/html")))
(defparameter *exit-on-/exit* nil
  "If T, exit when /exit request is received (on sbcl). This is used by tests to
shut down the server.")

(defmethod process-end-headers (connection (stream sample-server-stream))
  ;; do nothing
  )

(defmethod peer-ends-http-stream (connection (stream sample-server-stream))
  "Send some random payloads, or shut down the server."
  (cond
    ((equal "/" (get-path stream))
     (send-headers connection stream `((:status "200") ,@*headers-to-send*))
     (write-data-frame connection stream (map 'vector 'char-code *text-to-send*)
                       :end-stream t))
    #+sbcl
    ((equal "/exit" (get-path stream))
     (send-headers connection stream `((:status "200") ,@*headers-to-send*))
     (write-data-frame connection stream (map 'vector 'char-code (format nil "Goodbye"))
                       :end-stream t)
     (write-goaway-frame connection connection 0 +no-error+ #())
     (force-output (get-network-stream connection))
     (sb-ext:quit))
    (t
     (write-headers-frame connection stream `((:status "404") ("content-type" "text/html")) :end-headers t)
     (write-data-frame connection stream (map 'vector 'char-code "<h1>Not found</h1>")
                       :end-stream t)))
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
