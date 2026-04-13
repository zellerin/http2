(in-package http2/core)
(asdf:load-system "dot-stuff")

(defun draw-from-gv (source-path &optional
                                   (gv2 (namestring (make-pathname :type "gv2" :defaults source-path)))
                                   (svg (namestring (make-pathname :type "svg" :defaults source-path))))
  (uiop:run-program (list "gvpr" "-c" "-o"
                          gv2
                          "-f" (namestring (asdf:system-relative-pathname "http2" "doc/style.gvpr"))
                          (namestring source-path)))
  (uiop:run-program (list "dot" "-Tsvg" "-o" svg gv2)))

(defun update-graphs ()
  (draw-from-gv (dot-stuff:draw-classes (asdf:system-relative-pathname "http2" "doc/stream-errors.gv") '(http2/core:http-stream-error)))
  (draw-from-gv (dot-stuff:draw-classes (asdf:system-relative-pathname "http2" "doc/connection-errors.gv") '(http2/utils:communication-error)))
  (draw-from-gv (dot-stuff:draw-classes (asdf:system-relative-pathname "http2" "doc/connection-classes.gv") '(http2/core:http2-connection)
                                        '(http2/core:flow-control-mixin standard-object http2/core::test-buffered-stream)))
  (draw-from-gv (dot-stuff:draw-classes (asdf:system-relative-pathname "http2" "doc/stream-classes.gv") '(http2/core:http2-stream)
                                        '(http2/core:flow-control-mixin standard-object http2/core::test-buffered-stream)))
  (draw-from-gv (dot-stuff:draw-classes (asdf:system-relative-pathname "http2" "doc/dispatchers.gv") '(http2/server:tls-single-client-dispatcher)
                                        '(http2/core:flow-control-mixin standard-object http2/core::test-buffered-stream))))

(update-graphs)
