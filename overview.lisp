(in-package :http2)

;;;; This file provides skeleton for the documentation.  The documentation is in
;;;; the mgl-pax format. There are several outputs:
;;;;

(defparameter *pages*
  `(
    (:objects (, @overview)
     :uri-fragment "overview.html")
    (:objects (, @tutorials)
     :source-uri-fn ,(make-git-source-uri-fn "http2" "https://github.com/zellerin/http2"
                                             :git-version "v2-main")
     :uri-fragment "tutorials.html")

    (:objects (, @index)
     :source-uri-fn ,(make-git-source-uri-fn "http2" "https://github.com/zellerin/http2"
                                             :git-version "v2-main")
     :uri-fragment "index.html")))

(defun make-release-documentation ()
  "Make package documentation for the release:

- HTML documentation files,
- README to be distributed with the package

"
  (mgl-pax:update-asdf-system-readmes @overview "http2")
  (mgl-pax:update-asdf-system-html-docs @index "http2"
                                        :pages *pages*)
  (mgl-pax:update-asdf-system-html-docs @tutorials "http2"
                                        :pages `((:objects (, @index)
                                                  :source-uri-fn ,(make-git-source-uri-fn "http2"  "file:///Users/zellerin/projects/http2/"
                                                                                          :uri-format-string "~a/~*~A~*"))
                                                 (:objects (,)))
                                        :target-dir "/tmp/http2/")

  (mgl-pax:update-asdf-system-html-docs @overview "http2"
                                        :pages `((:objects (, @tutorials)
                                                  :source-uri-fn ,(make-git-source-uri-fn "http2"  "file:///Users/zellerin/projects/http2/"
                                                                                          :uri-format-string "~a/~*~A~*")))
                                        :target-dir "/tmp/http2/")

  (mgl-pax:update-asdf-system-html-docs @index "http2"
                                        :pages `((:objects (, @index)
                                                  :source-uri-fn ,(make-git-source-uri-fn "http2"  "file:///Users/zellerin/projects/http2/"
                                                                                          :uri-format-string "~a/~*~A~*")))
                                        :target-dir "/tmp/http2/"))

(defsection @overview
    (:title "Overview")
  "This is an HTTP/2 implementation in Common Lisp. It provides both high-level
interface as well as ways to fine tune its behaviour for better performance or
specific use cases.

For quick start, quickload \"HTTP2\" and see @TUTORIALS that show how to use a
simple client to fetch a resource or how to serve simple pages."
  (http2 asdf:system)
  (http2/client package)
  (http2/server package))

(defsection @index
    (:title "HTTP2 in Common Lisp")
  (@overview section)
  (@tutorials section)
#+nil  (http2/core::@frames-api section)
#+nil  (http2/core::@base-classes section)

#+nil  (@utils section)
#+nil  (@scheduling section)
#+nil  (#:http2 asdf/system:system)
#+nil  (http2/hpack::@hpack-api section)
#+nil  (http2::@buffer-stream-and-pipes section)
#+nil  (@terms section)
  (http2/core::@lisp-stream-emulation section))

(defsection @test
    ()
  (foobar foobar))

(defsection @tutorials
    (:title "Tutorials")
  (http2/client::@client section)
  (http2/server::@server section)
  (http2/server::@server-content section)
  (http2/server::@request-details section)
  (http2/client::@tutorial section)  )

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
