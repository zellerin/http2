(defpackage http2/client
  (:use #:cl #:http2)
  (:export #:retrieve-url))

(in-package http2/client)

(defclass sample-client-connection (http2::logging-connection http2::timeshift-pinging-connection)
  ((finished :accessor http2::get-finished :initarg :finished
             :initform nil))
  (:default-initargs :stream-class 'sample-client-stream))

(defclass sample-client-stream (logging-stream http2::body-collecting-mixin
                                http2::header-collecting-mixin)
  ((content-type :accessor get-content-type :initarg :content-type)))

(defun retrieve-url (url &key (method "GET"))
  "Retrieve URL through http/2 over TLS."
  (let ((parsed-url (puri:parse-uri url)))
    (with-http-connection (connection (puri:uri-host parsed-url)
                           :port (or (puri:uri-port parsed-url) 443)
                           :connection-class 'sample-client-connection)
      (send-headers connection :new
                    (request-headers method
                                     (or (puri:uri-path parsed-url) "/")
                                     (puri:uri-host parsed-url))
                               :end-stream t)
        ;; and test ping
#+nil      (dotimes (i 3)
        (send-ping connection))
      (wait-for-responses)
      (values (http2::get-body (first (http2::get-streams connection)))
              (http2::get-headers  (first (http2::get-streams connection)))))))

(defmethod peer-ends-http-stream ((connection sample-client-connection) stream)
  (terminate-locally connection))
