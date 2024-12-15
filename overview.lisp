(in-package :http2)

(defsection @overview
    (:title "Overview")
  "\\HTTP2 API library.

For quick start, it provides high level interface to \\HTTP2 as a client (@CLIENT) and
server (@SERVER). This provides a sane vanilla behaviour for client and dispatch-on-url based behaviour on server.

The vanilla behaviour can be configured and extended using generic functions
based @CALLBACK-API.

On background there is a @FRAMES-API and HTTP2/HPACK::@HPACK-API.")

(defsection @index
    (:title "HTTP2 in Common Lisp")
  (@overview section)
  (http2/client::@client section)
  (@tutorials section)
  (@frames-api section)
  (@server section)
  (@client section)
  (@base-classes section)

  (@utils section)
  (@scheduling section)
  (#:http2 asdf/system:system)
  (http2/hpack::@hpack-api section)
  (http2::@buffer-stream-and-pipes section)
  (@terms section))

(defsection @test
    ()
  (foobar foobar))

(defsection @tutorials
    (:title "Tutorials")
  (http2/client::@tutorial section)
  (http2/server-example::@hello-world-server section))

(in-package http2/server-example)
(mgl-pax:defsection @hello-world-server
    (:title "Hello World server")
  "You can start empty server on a port using a TLS key and certificate files:

```
(http2-server:start port)
```

It generates a fresh pair of private key and certificate on background, but of
course, you will want to provide your own certificates."

  "The server above does not serve anything useful (unless you consider 404 error useful).
Let us make it serve a \"Hello World\" web page. First, we define the handler to
serve the page for URL \"/\":

```
(define-exact-handler \"/1\"
    (send-text-handler \"/Hello World\"
                       :content-type \"text/plain; charset=UTF-8\"
                       :gzip nil))
```

In addition to the DEFINE-EXACT-HANDLER, there is also DEFINE-PREFIX-HANDLER
that serves the content when only prefix matches. SEND-TEXT-HANDLER takes a
string at handler definition time, compiles it to binary payload (i.e., UTF-8
conversion, compression), and serves the binary payload when requested by the
client. This is good for a static content."
  "However, most likely you want to generate at least some content
dynamically. Macro HANDLER allows this by providing a OUTPUT-STREAM to write to:

```
(define-exact-handler \"/2\"
    (handler (foo :utf-8 nil)
      (with-open-stream (foo foo)
        (send-headers
         '((:status \"200\")
           (\"content-type\" \"text/html; charset=utf-8\")))
        (format foo \"Hello World, this is random: ~a\" foo (random 10))))
```")

(in-package http2/client)

(mgl-pax:defsection @tutorial
    (:title "Build your own client")
  "Let us see what it takes to build simplified RETRIEVE-URL function from
components. It will use CL+SSL to build a Lisp stream over TLS stream over
network stream.

HTTP/2 requests are done over TLS connection created with an ALPN indication that it is to
be used for  HTTP/2. The helper function here is CONNECT-TO-TLS-SERVER, and then
WITH-OPEN-STREAM can be used:

```
  (defun my-retrieve-url (url)
    (let ((parsed-url (puri:parse-uri url)))
      (with-open-stream (network-stream
                         (connect-to-tls-server (puri:uri-host parsed-url)
                                                :sni (puri:uri-host parsed-url)
                                                :port (or (puri:uri-port parsed-url) 443)))
        (my-retrieve-url-using-network-stream network-stream url))))
```

Now that we have a Lisp STREAM to communicate over, we can establish HTTP/2
connection of class VANILLA-CLIENT-CONNECTION over it, send client request, and
then PROCESS-PENDING-FRAMES until server fully sends the response. That invokes
restart FINISH-STREAM with the processed stream that we handle. We can get the
data from it using DRAKMA-STYLE-STREAM-VALUES.

```
  (defun my-retrieve-url-using-network-stream (lisp-stream url)
    (with-http2-connection (connection 'vanilla-client-connection lisp-stream)
      (my-send-client-request connection url)
      (restart-case
          (process-pending-frames connection nil)
        (finish-stream (stream)
          (drakma-style-stream-values stream)))))
```

Sending the request involves creating a new HTTP2 stream with OPEN-HTTP2-STREAM
and proper parameters.

```
  (defun my-send-client-request (connection url)
    (open-http2-stream connection
                              (request-headers :GET (puri:uri-path (puri:parse-uri url))
                                               (puri:uri-host (puri:parse-uri url)))
                              :end-stream t))
```
"
  )
