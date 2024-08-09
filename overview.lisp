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
