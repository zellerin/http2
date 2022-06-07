(in-package http2)

;;;; Sample server with constant payload
(defclass sample-server-connection (server-http2-connection logging-connection)
  ()
  (:default-initargs :stream-class 'sample-server-stream))

(defclass sample-server-stream (logging-stream http2-stream)
  ((path    :accessor get-path    :initarg :path))
  (:default-initargs :path :undefined))

(defmethod add-header (stream (name (eql :path)) value)
  (setf (get-path stream) value))

(defparameter *text-to-send* (format nil "<h1>Hello World</h1>~&"))
(defparameter *headers-to-send* '(("content-type" "text/html")))

(defmethod process-end-headers (connection (stream sample-server-stream))
)

(defmethod peer-ends-http-stream (connection (stream sample-server-stream))
  (cond
    ((equal "/" (print (get-path stream)))
     (write-headers-frame connection stream `((:status 200) ,@*headers-to-send*)
                          :end-headers t)
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
                  (let ((tls-stream (cl+ssl:make-ssl-server-stream
                                     network-stream

                                     :certificate cert
                                     :key key)))
                    (unwind-protect
                         (process-server-stream tls-stream)
                      (close tls-stream)))
               (usocket:socket-close network-socket)
               (close network-stream))))
      (usocket:socket-close socket))))

;;;;
;;;; curl: send settings and wait for settings.
