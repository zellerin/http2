(in-package http2)

;;;; Sample server with constant payload
(defclass sample-server-connection (server-http2-connection logging-connection)
  ()
  (:default-initargs :stream-class 'sample-server-stream))

(defclass sample-server-stream (logging-stream http2-stream)
  ((path    :accessor get-path    :initarg :path))
  (:default-initargs :path :undefined))

(defmethod print-object ((stream sample-server-stream) out)
  (print-unreadable-object (stream out)
    (format out "#~d ~s ~s" (get-stream-id stream) (get-url stream) (get-state stream))))

(defmethod add-header ((stream sample-server-stream) (name (eql :path)) value)
  (setf (get-path stream) value))

(defparameter *text-to-send* (format nil "<h1>Hello World</h1>~&"))
(defparameter *headers-to-send* '(("content-type" "text/html")))

(defmethod process-end-headers (connection (stream sample-server-stream))
)

(defmethod peer-ends-http-stream (connection (stream sample-server-stream))
  (cond
    ((equal "/" (get-path stream))
     (send-headers connection stream `((:status 200) ,@*headers-to-send*))
     (write-data-frame connection stream (map 'vector 'char-code *text-to-send*)
                       :end-stream t))
    (t
     (write-headers-frame connection stream `((:status 404) ("content-type" "text/html")) :end-headers t)
          (write-data-frame connection stream (map 'vector 'char-code "<h1>Not found</h1>")
                       :end-stream t)))
  (force-output (get-network-stream connection)))

(defmethod peer-ends-http-stream (connection (stream sample-server-stream))
  (print (get-history stream)))

(defun process-server-stream (stream)
  (let ((connection (make-instance 'sample-server-connection
                                   :network-stream stream)))
    (with-simple-restart (close-connection "Close current connection")
      (handler-case
          (unwind-protect
               (progn
                 (let ((preface-buffer (make-array (length +client-preface-start+))))
                   (read-sequence preface-buffer stream)
                   (unless (equalp preface-buffer +client-preface-start+)
                     (warn "Client preface mismatch: got ~a" preface-buffer)))
                 (handler-case
                     (loop (read-frame connection))
                   (end-of-file () nil)))
            (progn
              (print (get-history connection))))
        (go-away ())))))

(defun wrap-to-tls-and-process-server-stream (raw-stream key cert)
  (let ((tls-stream (cl+ssl:make-ssl-server-stream
                     raw-stream
                     :certificate cert
                     :key key)))
    (unwind-protect
         (process-server-stream tls-stream)
      (close tls-stream))))

(defun create-server (port key cert)
  (let ((socket (usocket:socket-listen "127.0.0.1" port))
        (cl+ssl::*ssl-global-context* (cl+ssl::make-context)))
    (cl+ssl::ssl-ctx-set-alpn-select-cb  cl+ssl::*ssl-global-context*
                                         (cffi:get-callback 'cl+ssl::select-h2-callback))
    (unwind-protect
         (loop
           (let* ((network-socket (usocket:socket-accept socket :element-type '(unsigned-byte 8)))
                  (network-stream (usocket:socket-stream network-socket)))
             (unwind-protect
                  (wrap-to-tls-and-process-server-stream network-stream key cert)
               (usocket:socket-close network-socket))))
      (usocket:socket-close socket))))

;;;;
;;;; curl: send settings and wait for settings.
