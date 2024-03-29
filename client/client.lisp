;;;; Copyright 2022 by Tomáš Zellerin

(defpackage :http2/client
  (:use :cl :http2 :alexandria)
  (:export #:retrieve-url))

(in-package :http2/client)

(defun http-stream-to-vector (http2-stream)
  "Read HTTP2 raw stream payload data, do guessed conversions and return either
string or octets vector. You can expect the stream to be closed after calling
this."
  (let*  ((headers (get-headers http2-stream))
          (charset (http2::extract-charset-from-content-type (cdr (assoc "content-type" headers
                                                                         :test 'string-equal))))
          (encoded (equal "gzip" (cdr (assoc "content-encoding" headers
                                             :test 'string-equal)))))
    (with-open-stream (response-stream
                       (make-transport-stream http2-stream charset encoded))
      (if charset
          (read-stream-content-into-string response-stream)
          (read-stream-content-into-byte-vector response-stream)))))

(defun maybe-send-pings (connection ping)
  (typecase ping
    (integer (dotimes (i ping) (send-ping connection)))
    (null)
    (t (send-ping connection))))

(defun retrieve-url-using-connection (connection parsed-url
                                      &key
                                        (method "GET")
                                        content
                                        (content-fn (when content (curry #'write-sequence content)))
                                        additional-headers
                                        (content-type "text/plain; charset=utf-8")
                                        (charset (extract-charset-from-content-type content-type))
                                        gzip-content
                                        end-headers-fn
                                      &allow-other-keys)
  "HTTP2 stream object that represent a request sent on CONNECTION.

The stream does not necessarily contain response when returned. You can read its
headers after the end of headers is signalled (callback END-HEADERS-FN is
called) and until END-STREAM-FN is called, any reading of body may block.

- PARSED-URL is a parsed URL to provide (used for autority header and path)
- METHOD is a http method to use, as a symbol or string
- CONTENT-FN, if not null, should be a function of one argument, a stream, that
  sends data to the stream.
- providing CONTENT is a shorthand to provide CONTENT-FN that sends a sequence (string or binary)
- if CONTENT-TYPE is set, it is send in headers, and the stream for CONTENT-FN is of type derived from its associated charset as per EXTRACT-CHARSET-FROM-CONTENT-TYPE.
- if GZIP-CONTENT is set, the appropriate header is send, and the stream for
  CONTENT-FN is encrypted transparently."
  (let ((raw-stream
          (http2::open-http2-stream connection
                        (request-headers method
                                         (puri:uri-path parsed-url)
                                         (puri:uri-host parsed-url)
                                         :content-type content-type
                                         :gzip-content gzip-content
                                         :additional-headers additional-headers)
                        :end-stream (null (or content content-fn))
                        :stream-pars `(:end-headers-fn ,end-headers-fn))))
    (when content-fn
      (let ((out (make-transport-output-stream raw-stream charset nil)))
        (funcall content-fn out)
        (close out)))
    raw-stream))

(defun retrieve-url-using-network-stream (network-stream parsed-url
                                          &rest args
                                          &key (connection-class 'vanilla-client-connection)
                                            ping
                                          &allow-other-keys)
  "Open an HTTP/2 connection over NETWORK-STREAM and use it to request URL."

  (with-http2-connection (connection connection-class
                                     :network-stream network-stream)
    (maybe-send-pings connection ping)
    (apply #'retrieve-url-using-connection connection parsed-url args)
    (http2::process-pending-frames connection nil)
    (error "The stream never finished")))


(defun drakma-style-stream-values (raw-stream &key close-stream)
  "Return first few values as from DRAKMA:HTTP-REQUEST
- body of the reply
- status code as integer
- alist of headers
- the URL the reply came from (bogus value)
- the connection the reply comes from (not network stream)
- whether connection is closed (passed as parameter)
- reason phrase (bogus value)"
  (values
   (http-stream-to-vector raw-stream)
   (parse-integer (get-status raw-stream))
   (get-headers raw-stream)
   "/"
   (get-connection raw-stream)
   close-stream
   "HTTP2 does not provide reason phrases"))

(defun retrieve-url (url &rest pars
                     &key &allow-other-keys)
  "Retrieve URL through http/2 over TLS.

Ping peer and print round trip time if PING is set, repeatedly if this is a
number.

Send CONTENT if not NIL as payload that fits one frame, or call
CONTENT-FN (function of one parameter - output binary stream)."
  (let ((parsed-url (puri:parse-uri url)))
    (apply #'retrieve-url-using-network-stream
           (connect-to-tls-server (puri:uri-host parsed-url)
                                  :sni (puri:uri-host parsed-url)
                                  :port (or (puri:uri-port parsed-url) 443))
           parsed-url
           :end-headers-fn
           (lambda (raw-stream)
             (return-from retrieve-url
               (drakma-style-stream-values raw-stream)))
           pars)))
