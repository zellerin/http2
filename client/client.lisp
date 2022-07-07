;;;; Copyright 2022 by Tomáš Zellerin

(defpackage :http2/client
  (:use :cl :http2)
  (:export #:retrieve-url))



(in-package :http2/client)

(defun retrieve-url (url &key (method "GET") ((:verbose http2::*do-print-log*))
                           ping
                           additional-headers
                           content
                           content-fn
                           (content-encoding :utf-8))
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
                            (append
                             (request-headers method
                                              (or (puri:uri-path parsed-url) "/")
                                              (puri:uri-host parsed-url))
                             (mapcar 'http2::encode-header additional-headers))
                            :end-stream (null (or content content-fn))
                            :end-stream-callback (constantly t))))
        (typecase ping
          (integer
           (dotimes (i ping)
             (send-ping connection)))
          (null)
          (t (send-ping connection)))

        (cond
          (content
           (http2::write-data-frame stream
                                    content :end-stream t))
          (content-fn
           (with-open-stream (out
                              (flexi-streams:make-flexi-stream
                               stream
                               :external-format content-encoding))
             (funcall content-fn out))))
        (wait-for-responses)
        (values (http2::get-body stream)
                (http2::get-status stream)
                (http2::get-headers stream))))))

(defun input-stream-from-path
    (connection path hostname &key (method "GET") ((:verbose http2::*do-print-log*))
           additional-headers
           content-fn
           (content-encoding :utf-8))
  "Retrieve URL through http/2 over TLS.

Log events to standard output with VERBOSE set."
  (let ((stream
          (send-headers connection
                        (append
                         (request-headers method path hostname)
                         (mapcar 'http2::encode-header additional-headers))
                        :end-stream (null content-fn))))
    ;; send body
    (when content-fn
      (with-open-stream (out
                         (flexi-streams:make-flexi-stream
                          stream
                          :external-format content-encoding))
        (funcall content-fn out)))
    (make-instance 'http2::binary-input-stream-over-data-frames
                   :http-connection connection
                   :http-stream stream)))



(defun example-2 ()
  (with-http2-connection
      (connection
       'vanilla-client-io-connection
       :network-stream (http2/client::tls-connection-to "localhost" :port 1230))
           (with-open-stream (in (flexi-streams:make-flexi-stream (http2/client::input-stream-from-path connection "/ok" "localhost") :external-format :utf-8))
             (print (read-line in)))
           (with-open-stream (in (flexi-streams:make-flexi-stream (http2/client::input-stream-from-path connection "/ok" "localhost") :external-format :utf-8))
             (print (read-line in)))
           (with-open-stream (in (flexi-streams:make-flexi-stream (http2/client::input-stream-from-path connection "/" "localhost") :external-format :utf-8))
             (print (read-line in)))))
