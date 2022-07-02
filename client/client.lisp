;;;; Copyright 2022 by Tomáš Zellerin

(defpackage :http2/client
  (:use :cl :http2)
  (:export #:retrieve-url))



(in-package :http2/client)

(defun retrieve-url (url &key (method "GET") ((:verbose http2::*do-print-log*))
                           ping
                           additional-headers
                           content
                           content-fn)
  "Retrieve URL through http/2 over TLS.

Log events to standard output with VERBOSE set.

Ping peer and print round trip time if PING is set, repeatedly if this is a
number.

Send CONTENT if not NIL as payload that fits one frame, or call
CONTENT-FN (function of one parameter - output binary stream)."
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
                            :end-stream (null (or content content-fn))
                            :end-stream-callback (constantly t))))
        (cond
          (content
           (http2::write-data-frame connection stream
                                    content :end-stream t))
          (content-fn
           (with-open-stream (out (make-instance 'binary-output-stream-over-data-frames
                                                 :http-stream stream
                                                 :http-connection connection))
             (funcall content-fn out))))
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
