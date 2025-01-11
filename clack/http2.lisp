(mgl-pax:define-package #:clack.handler.http2
  (:use :cl :http2/server #:http2 #:mgl-pax)
  (:export :run)
  (:shadow :run))

(in-package clack.handler.http2)

(defsection @clack
    (:title "Implementing Clack interface"
     :export nil)
  "Clack is a web application environment that defines \"applications\" and allows
the to be run under various CL web servers.

Let us see how to make HTTP2 server usable under Clack, at least to some extent.

The full code is in clack/http2.lisp file."
  "You can run http2/server under clack by

```
    (clack:clackup
       (lambda (env)
         (declare (ignore env))
         '(200 (:content-type \"text/plain\") (\"Hello, Clack! Here we go!\")))
         :port 5052
         :server :http2)
```"
  (run function)
  (clack-server-connection class)
  (clack-server-stream class))

(defclass clack-server-connection (http2/server::vanilla-server-connection)
  ((app            :accessor get-app            :initarg :app)
   (peer-address   :accessor get-peer-address   :initarg :peer-address)
   (peer-port      :accessor get-peer-port      :initarg :peer-port))
  (:documentation "Extend vanilla-server-connection to store the application to use, and to keep
track of the client address and port."))

(defmethod initialize-instance :after ((connection clack-server-connection)
                                       &key &allow-other-keys)
  (with-slots (peer-port peer-address) connection
    (setf peer-port nil
          peer-address nil)))

(defclass clack-server-stream (http2/stream-overlay:http2-stream-with-input-stream http2/core:server-stream)
  ((request-headers :accessor get-request-headers
                    :initarg :request-headers))
  (:default-initargs :request-headers (make-hash-table :test 'equal))
  (:documentation "Stream class that collect headers to the hash table (as Clack expects)"))

(defmethod http2/core:add-header (connection (stream clack-server-stream) name value)
  (if (keywordp name)
      (call-next-method)
      (with-slots (request-headers) stream
        (setf (gethash name request-headers) value))))

(defmethod http2/core:peer-ends-http-stream ((stream clack-server-stream))
  (let ((response
          (funcall (get-app (http2/core::get-connection stream))
                   (let* ((parsed-uri (puri:parse-uri (http2::get-path stream)))
                          (server-name-and-port (http2/core::get-authority stream))
                          (colon-pos (position #\: server-name-and-port)))
                     (list
                      :request-method (http2/core::get-method stream)
                      :script-name ""
                      :path-info (puri:uri-path parsed-uri)
                      :query-string (puri:uri-query parsed-uri)
                      :url-scheme (http2/core::get-scheme stream)
                      :server-name (subseq server-name-and-port 0 colon-pos)
                      :server-port (if colon-pos
                                       (parse-integer
                                        (subseq server-name-and-port (1+ colon-pos)))
                                       443)
                      :server-protocol :http/2
                      :request-uri (http2::get-path stream)
                      :raw-body stream
                      :remote-addr (get-peer-address
                                    (http2/core::get-connection stream))
                      :remote-port (get-peer-port
                                    (http2/core::get-connection stream))
                      :headers (get-request-headers stream))))))
    (if (consp response)
        (destructuring-bind (status headers body) response
          (send-headers stream (cons (list :status (format nil "~d" status))
                                     (loop for (key value) on headers by 'cddr
                                           collect (list (string-downcase (symbol-name key))
                                                         (princ-to-string value)))))
          (with-open-stream (out (http2/core:make-transport-output-stream stream :utf8 nil))
            (etypecase body
              (cons ; list of streams
               (dolist (string body)
                 (princ string out)))
              (vector
               (write-sequence body out))
              (pathname
               (let ((buffer (http2/utils:make-octet-buffer 4096)))
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
      (with-open-stream (out (http2/core:make-transport-output-stream stream :utf8 nil))
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
  "Implement HTTP/2 server. Called by Clask when started with :server http2."
  (when fd (error "cannot listen on FD"))
  (http2/server:start port
                      :dispatcher (make-instance http2/server::*vanilla-server-dispatcher*
                                                 :private-key-file  ssl-key-file
                                                 :certificate-file ssl-cert-file
                                                 :connection-class 'clack-server-connection
                                                 :connection-args `(:app ,app
                                                                    :stream-class clack-server-stream))))
