(in-package http2)

(defclass client-connection (http2-connection)
  ((finished :accessor get-finished :initarg :finished
             :initform nil))
  (:default-initargs :stream-class 'client-stream))

(defclass client-stream (http2-stream body-collecting-mixin header-collecting-mixin)
  ((content-type :accessor get-content-type :initarg :content-type)
   (data         :accessor get-data         :initarg :data)))

(defun retrieve-url (url &key (method "GET"))
  "Retrieve URL through http/2 over TLS."
  (let ((parsed-url (puri:parse-uri url)))
    (with-http-connection (connection (puri:uri-host parsed-url)
                           :port (or (puri:uri-port parsed-url) 443)
                           :connection-class 'client-connection)
      (let ((http-stream (create-new-local-stream connection)))
        (write-headers-frame connection
                             http-stream
                             (request-headers method
                                              (or (puri:uri-path parsed-url) "/")
                                              (puri:uri-host parsed-url))
                             :end-headers nil :end-stream t
                             :padded #(0 0 0 0))
        ;; just to test continuation frame
        (write-continuation-frame connection
                                  http-stream
                                  (list (encode-header "user-agent" "CL/custom"))
                                  :end-headers t)
        ;; and test ping
        (write-ping-frame connection connection 12345)
        ;; and test go-away
        (write-goaway-frame connection connection 0 +no-error+ #() 0)
        (force-output (get-network-stream connection))
        (with-simple-restart (use-read-so-far "Use data read so far")
            (handler-case
                (loop
                  do
                     (read-frame connection)
                  until (get-finished connection))
              (end-of-file () nil)))
        (values (get-body http-stream)
              (get-headers http-stream))))))

(defmethod process-end-stream :after ((connection client-connection) stream)
  (setf (get-finished connection) t))
