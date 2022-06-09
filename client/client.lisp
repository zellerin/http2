(defpackage http2/client
  (:use #:cl #:http2)
  (:export #:retrieve-url))

(in-package http2/client)

(defclass sample-client-connection (client-http2-connection http2::history-printing-object
                                    http2::timeshift-pinging-connection)
  ((finished :accessor http2::get-finished :initarg :finished
             :initform nil))
  (:default-initargs :stream-class 'sample-client-stream))

(defclass sample-client-stream (client-stream ;; basic semantics and pseudoheaders
                                ;; recieved data are stored in slot BODY
                                http2::body-collecting-mixin
                                ;; headers (not pseudoheaders) are collected in
                                ;; slot HEADERS
                                http2::header-collecting-mixin
                                ;; when *do-print-log* is set, extensive event
                                ;; log is printed
                                http2::history-printing-object)
  ())

(defun retrieve-url (url &key (method "GET") ((:verbose http2::*do-print-log*))
                           ping
                           additional-headers
                           content)
  "Retrieve URL through http/2 over TLS.

Log events to standard output with VERBOSE set.

Ping peer and print round trip time if PING is set, repeatedly if this is a
number."
  (let ((parsed-url (puri:parse-uri url)))
    (with-http-connection (connection (puri:uri-host parsed-url)
                           :port (or (puri:uri-port parsed-url) 443)
                           :connection-class 'sample-client-connection)
      (let ((stream
              (send-headers connection :new
                            (append
                             (request-headers method
                                              (or (puri:uri-path parsed-url) "/")
                                              (puri:uri-host parsed-url))
                             (mapcar 'http2::encode-header additional-headers))
                                       :end-stream (null content))))
        (when content
          (http2::write-data-frame connection stream
                                   content :end-stream t))
        (typecase ping
          (integer
           (dotimes (i ping)
             (send-ping connection)))
          (null)
          (t (send-ping connection)))
        (wait-for-responses)
        (values (http2::get-body stream)
                (http2::get-status stream)
                (http2::get-headers stream))))))

(defmethod peer-ends-http-stream ((connection sample-client-connection) stream)
  (terminate-locally connection))
