;;;; Copyright 2022-2025 by Tomáš Zellerin

(in-package http2/server/threaded)


(mgl-pax:defsection @server-classes
    (:title "Server classes")
  (tls-dispatcher-mixin class)
  (single-client-dispatcher class)
  (tls-single-client-dispatcher class)
  (threaded-dispatcher class)
  (tls-threaded-dispatcher class))

(mgl-pax:defsection @server/threaded
    (:title "Threaded server")
  (make-http2-tls-context function)
  (create-https-server function)
  (@server-classes section)
  (@server-actions section))
