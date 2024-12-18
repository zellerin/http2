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
  (http2/core::@frames-api section)
  (http2/server::@server section)
  (http2/client::@client section)
  (http2/core::@base-classes section)

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
  (http2/client::@tutorial section))

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
