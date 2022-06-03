(in-package http2)

;;;; Sample server with constant payload
;;;; works with curl --http2-prior-knowledge http://localhost:<port>/

(defclass sample-server-connection (server-http2-connection logging-connection)
  ()
  (:default-initargs :stream-class 'sample-server-stream))

(defclass SAMPLE-SERVER-STREAM (LOGGING-STREAM http2-stream)
  ())

(defparameter *text-to-send* (format nil "Hello World~&"))
(defparameter *headers-to-send* '(("content-type" "text/plain")))

(defmethod process-end-headers (connection (stream sample-server-stream))
  (write-headers-frame connection stream `((:status 200) ,@*headers-to-send*) :end-headers t)
  (write-data-frame connection stream (map 'vector 'char-code *text-to-send*)
                    :end-stream t)
  (force-output (get-network-stream connection)))

(defun create-server (port)
  (let ((socket (usocket:socket-listen "127.0.0.1" port)))
    (unwind-protect
         (loop
           (let* ((network-socket (usocket:socket-accept socket :element-type '(unsigned-byte 8)))
                             (network-stream (usocket:socket-stream network-socket))
                  (connection (make-instance 'sample-server-connection
                                             :network-stream network-stream)))
             (with-simple-restart (close-connection "Close current connection")
               (unwind-protect
                    (progn
                      (let ((preface-buffer (make-array (length +client-preface-start+))))
                        (read-sequence preface-buffer network-stream)
                        (unless (equalp preface-buffer +client-preface-start+)
                          (warn "Client preface mismatch")))
                      (handler-case
                          (loop (read-frame connection))
                        (end-of-file () nil)))
                 (progn
                   (print (get-history connection))
                   (print (mapcar 'get-history (get-streams connection)))
                   (usocket:socket-close network-socket))))))
      (usocket:socket-close socket))))

;;;;
;;;; curl: send settings and wait for settings.
