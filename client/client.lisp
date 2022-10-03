;;;; Copyright 2022 by Tomáš Zellerin

(defpackage :http2/client
  (:use :cl :http2)
  (:export #:retrieve-url))

(in-package :http2/client)

(defun http-stream-to-vector (raw-stream)
  "Read HTTP2 raw stream payload data, do guessed conversions and return either
string or octets vector. You can expect the stream to be closed after calling
this."
  (with-open-stream (response-stream
                     (make-transport-stream
                      raw-stream
                      (get-headers raw-stream)))
    (if (typep response-stream
               'trivial-gray-streams:fundamental-character-stream)
        (alexandria:read-stream-content-into-string response-stream)
        (alexandria:read-stream-content-into-byte-vector response-stream))))

(defun retrieve-url-using-network-stream
    (network-stream parsed-url &key
                                 ((:verbose http2::*do-print-log*)
                                  http2::*do-print-log*)
                                 (connection-class 'vanilla-client-connection)
                                 content content-fn
                                 additional-headers
                                 ping
                                 (method "GET"))
  "Open an HTTP/2 connection over NETWORK-STREAM and use it to request URL."

  (let* ((connection
           (make-instance connection-class :network-stream network-stream))
         (raw-stream
           (send-headers connection
                         (request-headers method
                                          (puri:uri-path parsed-url)
                                          (puri:uri-host parsed-url)
                                          :additional-headers additional-headers)
                         :end-stream (null (or content content-fn)))))
    (typecase ping
      (integer (dotimes (i ping) (send-ping connection)))
      (null)
      (t (send-ping connection)))

    (when (or content-fn content)
      (let ((request-stream (make-transport-stream raw-stream
                                                   additional-headers)))
          (cond
            (content
             (http2::write-data-frame raw-stream content :end-stream t))
            (content-fn (funcall content-fn request-stream)
                        (http2::close-output raw-stream)))))
    (loop for parsed-frame = (http2::read-frame connection)
          until (eq parsed-frame :headers-frame))
    (unwind-protect
         (values
          (http-stream-to-vector raw-stream)
          (http2::get-status raw-stream)
          (http2::get-headers raw-stream))
      (http2::process-pending-frames connection)
      (close connection))))

(defun retrieve-url (url &rest pars
                     &key . #1=(method verbose
                                       ping
                                       additional-headers
                                       content
                                       content-fn
                                       stream-fn))
  "Retrieve URL through http/2 over TLS.

Log events with VERBOSE set.

Ping peer and print round trip time if PING is set, repeatedly if this is a
number.

Send CONTENT if not NIL as payload that fits one frame, or call
CONTENT-FN (function of one parameter - output binary stream)."
  (declare (ignore . #1#))
  (let ((parsed-url (puri:parse-uri url)))
    (apply #'retrieve-url-using-network-stream
           (tls-connection-to (puri:uri-host parsed-url)
                              :sni (puri:uri-host parsed-url)
                              :port (or (puri:uri-port parsed-url) 443))
           parsed-url
           pars)))
