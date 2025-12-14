;;;; Copyright 2022-2025 by Tomáš Zellerin

(in-package :http2/client)

(named-readtables:in-readtable
            pythonic-string-reader:pythonic-string-syntax)

(defsection @client-api (:title "HTTP/2 client API")
  "There is a simple client in the package http2/client."
  (retrieve-url function)
  (request-headers function)
  (drakma-style-stream-values function))

(defun request-headers (method path authority
                        &key (scheme "https")
                          content-type
                          gzip-content
                          additional-headers)
  "Encode standard request headers. The obligatory headers are passed as the
positional arguments. ADDITIONAL-HEADERS are a list of conses, each containing
header name and value."
  `((:method, (if (symbolp method) (symbol-name method) method))
    (:scheme ,scheme)
    (:path ,(or path "/"))
    (:authority ,authority)
    ,@(when content-type
        `(("content-type" ,content-type)))
    ,@(when gzip-content
        '(("content-encoding" "gzip")))
    ,@(mapcar (lambda (a)
                (list (car a) (cdr a)))
              additional-headers)))

(defsection @customizing-client-example-multi
    (:title "Client example: multiple requests")


  """
For example, this is how you can make a client that download several pages from
same source.

First, you need a custom request class, both to have something to specialize
FETCH-RESOURCE on and to keep the list of resources to fetch.

```
(defclass multi-url-request (simple-request)
  ((urls :accessor get-urls :initarg :urls)))

(defmethod fetch-resource ((connection client-http2-connection)
                           (request multi-url-request) args)

  (dolist (r (get-urls request))
    (fetch-resource connection r nil)))
```

Then you need a specialized stream class to specialize what to do when the
responses arrive. Here it prints out the response body for simplicity, and ends
if it got the last response.

```
(defclass my-client-stream (vanilla-client-stream)
  ())

(defmethod peer-ends-http-stream ((stream my-client-stream))
  (print (or (get-body stream) (http-stream-to-string stream)))
  (when (null (get-streams (get-connection stream)))
    (signal 'client-done))
  (terpri))
```

See GET-BODY,  HTTP-STREAM-TO-STRING and CLIENT-DONE.

And finally, you need to pass these as the call parameter:

```
(http2/client:retrieve-url "https://localhost:8088/body"
   :request-class 'multi-url-request
   :urls '("https://localhost:8088/body" "https://localhost:8088/")
   :stream-class 'my-client-stream)
```
"""
  (get-connection (method (http2-stream-minimal)))
  (get-streams (method (stream-collection))))

(defsection @customizing-client
    (:title "Customize client")
  "RETRIEVE-URL is in fact a thin wrapper over FETCH-RESOURCE generic function."
  (fetch-resource generic-function)
  " You can customize its behaviour by creating subclasses for the GENERIC-REQUEST
 class and specialized methods for your new classes, as well as by changing
 documented variables."
  (@customizing-client-example-multi section)
  (@customizing-client-reference section))

(defsection @customizing-client-reference
    (:title "Client reference")
  "For a simple request without body, following documented methods are called in
sequence:"
  (fetch-resource (method (t string t)))
  (fetch-resource (method (t puri:uri t)))
  (fetch-resource (method (stream generic-request t)))
  (fetch-resource (method (client-http2-connection generic-request t)))
  (fetch-resource (method (client-http2-connection request-with-utf8-body t)))
  (fetch-resource (method (client-http2-connection request-with-binary-body t)))
  (generic-request class)
  (simple-request class)
  (request-with-body class)
  (request-with-binary-body class)
  (request-with-utf8-body class)
  (*default-client-connection-class* variable)
  (client-done condition))

(defclass generic-request ()
  ((uri     :accessor get-uri     :initarg :uri)
   (method  :accessor get-method  :initarg :method)
   (headers :accessor get-headers :initarg :headers)
   (no-body :accessor get-no-body :initarg :no-body))
  (:default-initargs :method "GET" :headers nil)
  (:documentation
   "Base class for requests. It has slots for URI, HTTP METHOD, HEADERS (some headers
are implicit) and NO-BODY flag to determine whether there would be data frames
sent."))

(defclass simple-request (generic-request)
  ()
  (:default-initargs :method "GET" :headers nil
   :no-body t)
  (:documentation "Class for requests with no body. Implies GET method by default."))

(defclass request-with-body (generic-request)
  ((content      :accessor get-content      :initarg :content)
   (content-type :accessor get-content-type :initarg :content-type)
   (gzip-content :accessor get-gzip-content :initarg :gzip-content))
  (:default-initargs :no-body nil :method "POST")
  (:documentation
   "Base class for requests with a body. Implies POST method by default. Some method
for FETCH-RESOURCE must be defined to actually send the content in data frames."))

(defclass request-with-utf8-body (request-with-body)
  ()
  (:default-initargs :content-type "text/plain; charset=utf-8"))

(defclass request-with-binary-body (request-with-body)
  ()
  (:default-initargs :content-type "application/octet-stream"))

(defvar *default-client-connection-class* 'vanilla-client-connection
  "Default class to be used for new connections in FETCH-RESOURCE.")

(defmethod get-headers ((request request-with-body))
  `(("content-type" . ,(get-content-type request))
    ,@(call-next-method)))

(defgeneric fetch-resource (medium url pars)
  (:documentation "Retrieve URL over some medium - HTTP/2 connection, network socket, .....

The ARGS is a property list used by some methods and ignored/passed down by others.")

  (:method (medium (url string) args)
    "Parse URL into PURI:URI object and fetch the resource using that."
    (fetch-resource medium (puri:parse-uri url) args))

  (:method (medium (url puri:uri) args)
    "Convert URL into an instance of SIMPLE-REQUEST, REQUEST-WITH-UTF8-BODY or REQUEST-WITH-BINARY-BODY class.

Pass ARGS to the MAKE-INSTANCE call.

If ARGS (a property list) has CONTENT property, check its type - if it is a
string, REQUEST-WITH-UTF8-BODY is created, if a simple or octet vector,
REQUEST-WITH-BINARY-BODY, if not present or nil SIMPLE-REQUEST. If it is
something else, behaviour is undocumented.

Note that some of the methods actually wait to get the responses."
    (fetch-resource medium
                    (apply #'make-instance
                           (getf args :request-class
                                 (etypecase (getf args :content)
                                   (null 'simple-request)
                                   (string 'request-with-utf8-body)
                                   ((or simple-vector  octet-vector) 'request-with-binary-body)))
                           :uri url
                           :allow-other-keys t
                           args)
                    args))

  (:method ((medium (eql :connect)) (request generic-request) args)
    "Not part of API."
    (with-slots ((parsed-url uri)) request
      (let ((network-stream
              (connect-to-tls-server (puri:uri-host parsed-url)
                                     :sni (puri:uri-host parsed-url)
                                     :port (or (puri:uri-port parsed-url) 443))))
        (unwind-protect
             (fetch-resource network-stream request args)
          (handler-case
              (close network-stream)
            (cl+ssl::ssl-error-zero-return ()))))))

  (:method ((network-stream stream) (request generic-request) args)
    "Open HTTP/2 connection over the STREAM and fetch the resource using this connection.

*Wait for the pending frames to actually receive the response*.

Class of the new connection is taken from :CONNECTION-CLASS property of ARGS,
falling back to *DEFAULT-CLIENT-CONNECTION-CLASS*. ARGS are passed to the MAKE-INSTANCE."
    (let ((connection
       (apply #'make-instance (getf args :connection-class *default-client-connection-class*)
                      :network-stream network-stream
                      args)))
      (fetch-resource connection request args)
      (handler-case
          (process-pending-frames connection)
        (http-stream-error (e)
          ;; promote stream error that is usually just warning to an error
          (error e)))))

  (:method ((connection client-http2-connection) (request generic-request) args)
    "Open the new stream by sending headers frame to the server.

Details of the frame are taken from the REQUEST instance.

Return the new stream."
    (send-headers (create-new-local-stream connection nil)
                  (request-headers
                   (get-method request)
                   (puri:uri-path (get-uri request))
                   (puri:uri-host (get-uri request))
                   :additional-headers
                   (append (get-headers request)
                           (getf args :additional-headers)))
                  :end-stream (get-no-body request)
                  :end-headers t))

  (:method ((connection client-http2-connection) (request request-with-utf8-body) args)
    "Open the HTTP/2 stream and send out the content as UTF-8."
    (with-open-stream (out (make-transport-output-stream (call-next-method)
                                                         :utf-8 nil))
      (write-sequence (get-content request) out)))

  (:method ((connection client-http2-connection) (request request-with-binary-body) args)
    "Open the HTTP/2 stream and send out the content as octets sequence."
    (with-open-stream (out (make-transport-output-stream (call-next-method)
                                                         nil nil))
      (write-sequence (get-content request) out))))

(defun drakma-style-stream-values (raw-stream &key close-stream)
  "Return values as from DRAKMA:HTTP-REQUEST. Some of the values are meaningless,
but kept for compatibility purposes."
  (values
   (or (get-body raw-stream) (http-stream-to-string raw-stream))
   (parse-integer (get-status raw-stream))
   (get-headers raw-stream)
   "/"
   (get-connection raw-stream)
   close-stream
   "HTTP2 does not provide reason phrases"))

(define-condition client-done (condition)
  ((result :accessor get-result :initarg :result
           :initform nil))
  (:documentation
   "Handled by RETRIEVE-URL. The client should signal it when the processing is
done."))

(defgeneric present-result (result)
  (:method (result) result)
  (:method ((result client-stream))
    (drakma-style-stream-values result)))

(defun retrieve-url (url &rest pars
                     &key
                       method content content-fn additional-headers
                       content-type charset gzip-content
                     &allow-other-keys)
  "Retrieve URL (a string) through HTTP/2 over TLS.

See FETCH-RESOURCE for documentation of the keyword parameters.

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
==> #<VANILLA-CLIENT-CONNECTION >
==> NIL
==> \"HTTP2 does not provide reason phrases\"
```

The individual values are:

- body of the reply (response data) - string or array
- status code as integer
- alist of response headers
- the URL the reply came from (bogus value for Drakma compatibility)
- the connection the reply comes from (not network stream as in Drakma, but same purpose - can be reused for ruther queries.)
- whether connection is closed (passed as parameter)
- reason phrase (bogus value)"
  ;; parameters are just for documentation purposes
  (declare (ignore method content content-fn
                   content-type charset gzip-content additional-headers))
  (unwind-protect
       (handler-case
           (fetch-resource :connect url pars)
         (client-done (c)
           (present-result (get-result c))))))
