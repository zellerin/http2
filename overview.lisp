(in-package :http2)

(mgl-pax:defsection @overview
    (:title "HTTP2 in Common Lisp")
  (http2::@frames-api mgl-pax:section)
  (http2/hpack::@hpack-api mgl-pax:section)
  (http2::@utils mgl-pax:section)
  (http2::@callbacks mgl-pax:section)
  (http2::@client mgl-pax:section)
  (http2::@terms mgl-pax:section))

; (mgl-pax:update-asdf-system-html-docs http2::@overview :http2)
