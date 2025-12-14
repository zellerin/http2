(in-package :http2)

(defsection @overview (:title "Overview")
  "This is an HTTP/2 implementation in Common Lisp. It provides both high-level
interface as well as components to build an optimized tools for specific use cases.

For quick start, quickload [HTTP2][asdf:system] and see @TUTORIALS that show how to use a
simple client to fetch a resource or how to start the server and serve some
content.

For more documentation consult @REFERENCE.")

(defsection @index
    (:title "HTTP2 in Common Lisp")
  (@overview section)
  (@tutorials section)
  (@reference section)
  (@implementation section))

(defsection @tutorials (:title "Tutorials")
  (http2/client::@client-tutorials section)
  (@server-tutorials section))

(defsection @server-tutorials (:title "Server tutorials")
  "Server related interfaces are exported from the HTTP2/SERVER package and are
part of the HTTP2/SERVER system. This system is also loaded when HTTP2 is loaded."
  (http2/server::@server-start-stop section)
  (http2/server::@server-content section)
  (http2/server::@request-details section)
  (http2/server package)
  (http2/server asdf:system))

(defsection @reference (:title "API documentation")
  (@systems-and-packages section)
  (http2/client::@client-api section)
  (http2/server::@server-reference section)
  (http2/server::@logging section))

(defsection @systems-and-packages (:title "Systems and packages")
    (http2 asdf:system)
  (http2/client package))

(defsection @implementation
    (:title "Implementation details (not part of API)")

  "Here are some notes that try to explain how are things implemented. If this is
out of sync or if it does not make sense at all, fixing it is not a hight
priority, and definitely not a blocker."
  (http2/core::@implementation/overview section)
  (http2/hpack::@hpack-api section)
  (http2/core::@data section)
  (http2/server::@server-reference section)
  (http2/server/poll::@async-server section))

(defsection @test
    ()
  (foobar foobar))

(defun pages ()
  `(#+nil (:objects ()
     :uri-fragment "overview.html"
     :source-uri-fn ,(make-git-source-uri-fn "http2" "https://github.com/zellerin/http2" :git-version "master"))
    (:source-uri-fn ,(make-git-source-uri-fn "http2" "https://github.com/zellerin/http2"
                                             :git-version "master")
     :uri-fragment "index.html")

    (:objects (, @reference)
     :source-uri-fn ,(make-git-source-uri-fn "http2" "https://github.com/zellerin/http2"
                                             :git-version "master")
     :uri-fragment "reference.html")
    (:objects (, @implementation)
     :source-uri-fn ,(make-git-source-uri-fn "http2" "https://github.com/zellerin/http2"
                                             :git-version "master"))))

(defun make-release-documentation ()
  "Make package documentation for the release:

- HTML documentation files,
- README to be distributed with the package

"
  (mgl-pax:update-asdf-system-readmes @overview "http2")
  (mgl-pax:update-asdf-system-html-docs @index "http2" :pages (pages)))
