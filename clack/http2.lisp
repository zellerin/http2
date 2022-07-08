(cl:defpackage clack.handler.http2
  (:use :cl :http2)
  (:export :run))

(in-package clack.handler.http2)

(defvar *app*
  nil "Value to initialize app. Needed before http2 has new version that allows to pass it as a parameter.")

(defclass clack-server-connection (http2::server-http2-connection http2::history-printing-object)
  ((app          :accessor get-app          :initarg :app)
   (peer-address :accessor get-peer-address :initarg :peer-address)
   (peer-port    :accessor get-peer-port    :initarg :peer-port))
  (:default-initargs :stream-class 'clack-server-stream
                     :app *app*))

(defmethod initialize-instance :after ((connection clack-server-connection)
                                       &key http2::network-stream &allow-other-keys)
  (with-slots (peer-port peer-address) connection
    (setf peer-port nil
          peer-address nil)))

(defclass clack-server-stream (http2::server-stream binary-output-stream-over-data-frames
                               http2::history-printing-object)
  ((request-headers :accessor get-request-headers
                    :initarg :request-headers))
  (:default-initargs :request-headers (make-hash-table :test 'equal)))

(defmethod http2::add-header (connection (stream clack-server-stream) name value)
  (if (keywordp name)
      (call-next-method)
      (with-slots (request-headers) stream
        (setf (gethash name request-headers) value))))

(defmethod http2::peer-ends-http-stream ((stream clack-server-stream))
  (destructuring-bind (status headers body)
      (print (funcall (get-app (http2::get-connection stream))
                      (print
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
    (send-headers stream (cons (list :status (format nil "~d" status))
                               (loop for (key value) on headers by 'cddr
                                     collect (list (string-downcase (symbol-name key))
                                               value))))
    (etypecase body
      (cons ; list of streams
       (with-open-stream (out (flexi-streams:make-flexi-stream stream))
         (dolist (string body)
           (princ string out)))))))

(defun run (*app* &key debug port ssl-key-file ssl-cert-file fd)
  (when fd (error "cannot listen on FD"))
  (http2::create-https-server port ssl-key-file ssl-cert-file
                              :verbose debug
                              :connection-class 'clack-server-connection))
