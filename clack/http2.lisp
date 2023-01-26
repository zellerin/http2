(cl:defpackage clack.handler.http2
  (:use :cl :http2)
  (:export :run))

(in-package clack.handler.http2)

(defclass clack-server-connection (http2::server-http2-connection)
  ((app          :accessor get-app          :initarg :app)
   (peer-address :accessor get-peer-address :initarg :peer-address)
   (peer-port    :accessor get-peer-port    :initarg :peer-port))
  (:default-initargs :stream-class 'clack-server-stream))

(defmethod initialize-instance :after ((connection clack-server-connection)
                                       &key &allow-other-keys)
  (with-slots (peer-port peer-address) connection
    (setf peer-port nil
          peer-address nil)))

(defclass clack-server-stream (http2::http2-stream-with-input-stream http2::server-stream)
  ((request-headers :accessor get-request-headers
                    :initarg :request-headers))
  (:default-initargs :request-headers (make-hash-table :test 'equal)))

(defmethod http2::add-header (connection (stream clack-server-stream) name value)
  (if (keywordp name)
      (call-next-method)
      (with-slots (request-headers) stream
        (setf (gethash name request-headers) value))))

(defmethod http2::peer-ends-http-stream ((stream clack-server-stream))
  (let ((response
          (funcall (get-app (http2::get-connection stream))
                   (let* ((parsed-uri (puri:parse-uri (http2::get-path stream)))
                          (server-name-and-port (http2::get-authority stream))
                          (colon-pos (position #\: server-name-and-port)))
                     (list
                      :request-method (http2::get-method stream)
                      :script-name ""
                      :path-info (puri:uri-path parsed-uri)
                      :query-string (puri:uri-query parsed-uri)
                      :url-scheme (http2::get-scheme stream)
                      :server-name (subseq server-name-and-port 0 colon-pos)
                      :server-port (if colon-pos
                                       (parse-integer
                                        (subseq server-name-and-port (1+ colon-pos)))
                                       443)
                      :server-protocol :http/2
                      :request-uri (http2::get-path stream)
                      :raw-body stream
                      :remote-addr (get-peer-address
                                    (http2::get-connection stream))
                      :remote-port (get-peer-port
                                    (http2::get-connection stream))
                      :headers (get-request-headers stream))))))
    (if (consp response)
        (destructuring-bind (status headers body) response
          (send-headers stream (cons (list :status (format nil "~d" status))
                                     (loop for (key value) on headers by 'cddr
                                           collect (list (string-downcase (symbol-name key))
                                                         (princ-to-string value)))))
          (with-open-stream (out (make-transport-output-stream stream :utf8 nil))
            (etypecase body
              (cons ; list of streams
               (dolist (string body)
                 (princ string out)))
              (vector
               (write-sequence body out))
              (pathname
               (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
                 (with-open-file (in body :element-type '(unsigned-byte 8))
                   (loop for len = (read-sequence buffer in)
                         while (plusp len)
                         do
                            (write-sequence buffer out :end len))))))))
        (funcall response (http2-responder stream)))))

;; this API looks a bit crazy, lambdas in lambdas...
(defun http2-responder (stream)
  "Responder for a stream"
  (lambda (status-and-headers)
    (destructuring-bind (status headers) status-and-headers
        (send-headers stream (cons (list :status (format nil "~d" status))
                                   (loop for (key value) on headers by 'cddr
                                         collect (list (string-downcase (symbol-name key))
                                                       value)))))
    (lambda (body &key (start 0) (end (length body)) &allow-other-keys)
      (with-open-stream (out (make-transport-output-stream stream :utf8 nil))
        (etypecase body
          (string
           (write-sequence
            (flex:string-to-octets body
                                   :start start :end end
                                   :external-format :utf-8)
            out))
          (vector (write-sequence body out :start start :end end))
          (null))))))

(defvar *default-certificate-pair*
  '("/tmp/server.key" "/tmp/server.crt")
  "Path to files with the default private key and certificate to use for server.")

(defun run (app &key port (ssl-key-file (first *default-certificate-pair*))
                    (ssl-cert-file (second *default-certificate-pair*))
                    fd)
  (when fd (error "cannot listen on FD"))
  (http2:create-https-server port ssl-key-file ssl-cert-file
                             :connection (make-instance 'clack-server-connection
                                                        :app app)))
