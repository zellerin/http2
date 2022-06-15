;;;; Copyright 2022 by Tomáš Zellerin

(defpackage :http2/client
  (:use :cl :http2)
  (:export #:retrieve-url))



(in-package :http2/client)

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
                           :port (or (puri:uri-port parsed-url) 443))
      (let ((stream
              (send-headers connection
                            :new
                            (append
                             (request-headers method
                                              (or (puri:uri-path parsed-url) "/")
                                              (puri:uri-host parsed-url))
                             (mapcar 'http2::encode-header additional-headers))
                            :end-stream (null content)
                            :end-stream-callback (constantly t))))
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
