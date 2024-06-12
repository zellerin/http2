;;;; Copyright 2022 by Tomáš Zellerin

(mgl-pax:define-package :http2/client
  (:use :cl :http2 :alexandria)
  (:export #:retrieve-url))

(in-package :http2/client)

(mgl-pax:defsection @client
  (:title "Client sample implementation")
  "There is a simple client in the package http2/client."
  (retrieve-url function)
  (drakma-style-stream-values function)
  (retrieve-url-using-http-connection function))

(defun maybe-send-pings (connection ping)
  (typecase ping
    (integer (dotimes (i ping) (send-ping connection)))
    (null)
    (t (send-ping connection))))

(defun retrieve-url-using-http-connection
    (http-connection parsed-url
     &key
       (method "GET")
       content additional-headers
       (content-fn (when content (curry #'write-sequence content)))
       (content-type "text/plain; charset=utf-8")
       (charset (extract-charset-from-content-type content-type))
       gzip-content end-headers-fn end-stream-fn &allow-other-keys)
  "Return HTTP-STREAM object that represent a request sent on HTTP-CONNECTION.

The stream does not necessarily contain response when returned. You can read its
headers after the end of headers is signalled (callback END-HEADERS-FN is
called) and until END-STREAM-FN is called, any reading of body may block.

Parameters:

- PARSED-URL is a parsed URL to provide (used for autority header and path)
- METHOD is a http method to use, as a symbol or string
- CONTENT-FN, if not null, should be a function of one argument, a stream, that
  sends data to the stream.
- providing CONTENT is a shorthand to provide CONTENT-FN that sends a sequence (string or binary)
- if CONTENT-TYPE is set, it is send in headers, and the stream for CONTENT-FN is of type derived from its associated charset as per EXTRACT-CHARSET-FROM-CONTENT-TYPE.
- if GZIP-CONTENT is set, the appropriate header is send, and the stream for
  CONTENT-FN is compressed transparently."
  (let ((raw-stream
          (http2::open-http2-stream http-connection
                        (request-headers method
                                         (puri:uri-path parsed-url)
                                         (puri:uri-host parsed-url)
                                         :content-type content-type
                                         :gzip-content gzip-content
                                         :additional-headers additional-headers)
                        :end-stream (null (or content content-fn))
                        :stream-pars `(:end-headers-fn ,end-headers-fn :end-stream-fn ,end-stream-fn))))
    (when content-fn
      (let ((out (make-transport-output-stream raw-stream charset nil)))
        (funcall content-fn out)
        (close out)))
    raw-stream))

(defun make-transport-output-stream (http2-stream charset gzip)
  "An OUTPUT-STREAM built atop RAW STREAM with transformations based on HEADERS."
  (let* ((transport (make-instance 'http2::payload-output-stream :base-http2-stream http2-stream)))
    (when gzip
      (setf transport (gzip-stream:make-gzip-output-stream transport)))
    (when charset
      (setf transport
            (flexi-streams:make-flexi-stream
             transport
             :external-format charset)))
    transport))



(defun http-stream-to-vector (http-stream)
  ;; 20240611 TODO: document
  (with-output-to-string (*standard-output*)
    (mapc 'princ (nreverse (http2::get-text http-stream)))))

(defmethod http2::apply-text-data-frame ((stream vanilla-client-stream) text)
  (push text (http2::get-text stream)))

(defun retrieve-url-using-network-stream (network-stream parsed-url
                                          &rest args
                                          &key (connection-class 'vanilla-client-connection)
                                            ping
                                          &allow-other-keys)
  "Open an HTTP/2 connection over NETWORK-STREAM and use it to request URL."

  (with-http2-connection (connection connection-class
                                     :network-stream network-stream)
    (maybe-send-pings connection ping)
    (apply #'retrieve-url-using-http-connection connection parsed-url args)
    (restart-case
             (http2::process-pending-frames connection nil)
           (finish-stream (stream)
             (drakma-style-stream-values stream)))))


(defun drakma-style-stream-values (raw-stream &key close-stream)
  "Return values as from DRAKMA:HTTP-REQUEST. Some of the values are meaningless,
but kept for compatibility purposes.

- body of the reply
- status code as integer
- alist of headers
- the URL the reply came from (bogus value)
- the connection the reply comes from (not network stream as in Drakma, but same purpose - can be reused for ruther queries.)
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
                     &key
                       method content content-fn additional-headers
                       content-type charset gzip-content
                     &allow-other-keys)
  "Retrieve URL (a string) through HTTP/2 over TLS.

See RETRIEVE-URL-USING-CONNECTION for documentation of the keyword parameters.

Example:

```
(http2/client:retrieve-url \"https://example.com\")
==> \"<!doctype html>
... <html>
... <head>
...     <title>Example Domain</title>
...
...     <meta charset=\"utf-8\" />
...     <meta http-equiv=\"Content-type\" content=\"text/html; charset=utf-8\" />
...     <meta name=\"viewport\" conten...[sly-elided string of length 1256]\"
==> 200 (8 bits, #xC8, #o310, #b11001000)
==> ((\"content-length\" . \"1256\") (\"x-cache\" . \"HIT\") (\"vary\" . \"Accept-Encoding\")
...  (\"server\" . \"ECS (bsb/27E0)\")
...  (\"last-modified\" . \"Thu, 17 Oct 2019 07:18:26 GMT\")
...  (\"expires\" . \"Thu, 28 Sep 2023 19:38:44 GMT\")
...  (\"etag\" . \"\\\"3147526947+ident\\\"\") (\"date\" . \"Thu, 21 Sep 2023 19:38:44 GMT\")
...  (\"content-type\" . \"text/html; charset=UTF-8\")
...  (\"cache-control\" . \"max-age=604800\") (\"age\" . \"151654\"))
==> \"/\"
==> #<HTTP2:VANILLA-CLIENT-CONNECTION >
==> NIL
==> \"HTTP2 does not provide reason phrases\"
```

See DRAKMA-STYLE-STREAM-VALUES for meaning of the individual values
"
  ;; parameters are just for documentation purposes
  (declare (ignore method content content-fn additional-headers
                   content-type charset gzip-content))
  (let ((parsed-url (puri:parse-uri url)))
    (apply #'retrieve-url-using-network-stream
           (connect-to-tls-server (puri:uri-host parsed-url)
                                  :sni (puri:uri-host parsed-url)
                                  :port (or (puri:uri-port parsed-url) 443))
           parsed-url
           :end-headers-fn (constantly nil)
           :end-stream-fn
           (lambda (raw-stream)
             (invoke-restart 'finish-stream raw-stream))
           pars)))
