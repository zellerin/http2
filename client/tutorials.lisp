(in-package http2/client)

(defsection @client-tutorials (:title "Client tutorials")
  "The client tutorials show how to fetch a web resource using built-in
Drakma-style interface, and how to make more advanced client that does several
requests in parallel using HTTP/2 streams."
  (@client-tutorial-simple section)
  (@tutorial-build-client section))

(defsection @client-tutorial-simple
    (:title "Using built-in HTTP/2 client")
  "You can use RETRIEVE-URL to fetch a a web resource.

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

 It was designed to be similar to DRAKMA:HTTP-REQUEST, but is not completely
same. See below what the function does, some notable differences are:

- The last parameter (response reason) is no longer relevant in HTTP/2. It is a
  dummy string that might be removed in further releases.

- The path is taken as-is. You cannot provide GET parameters to fill in.

- The redirects are not handled transparently. What is returned is returned.

- It is your responsibility to make sure that the HTTP/2 headers are
  correct (lowercase, order).

- You cannot get stream on output to work with. You get the connection, and you
  can use it.

Basically, the differences fall into two areas, that it does not (yet) provide
all the features of Drakma, and HTTP/2 is different.")

(defsection @tutorial-build-client
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
```")
