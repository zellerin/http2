(in-package :http2)

(defsection @overview
    (:title "Overview")
  "\\HTTP2 API library.

First, it has some high level interface to \\HTTP2 @CLIENT and
@SERVER; default is a sane vanilla behaviour. On this level, The communication is done mostly by writing to and reading from (Lisp) streams and sending/parsing/receiving headers.

The vanilla behaviour can be configured by @PARAMETERS and extended with @CALLBACK-API.
This is an object oriented interface that allows to customize how the client or server acts in some situations by extending generic functions.

On background there is a @FRAMES-API and HTTP2/HPACK::@HPACK-API.")

(defsection @index
    (:title "HTTP2 in Common Lisp")
  (@overview section)
  (@terms section)
  (@utils section)
  (@callbacks section)
  (@client section)
  (http2/client::@client section)
  (@server/threaded section)
  (@scheduling section)
  (#:http2 asdf/system:system)
  (@frames-api section)
  (http2/hpack::@hpack-api section))

; (mgl-pax:update-asdf-system-html-docs http2::@index :http2)

(defsection @test
    ()
  (foobar foobar))
