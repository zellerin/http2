;;;; Copyright 2022-2025 by Tomáš Zellerin

(in-package http2/server/threaded)


(defsection @server-classes (:title "Server classes")
  (tls-dispatcher-mixin class)
  (single-client-dispatcher class)
  (tls-single-client-dispatcher class)
  (threaded-dispatcher class)
  (tls-threaded-dispatcher class))

(defsection @server/threaded (:title "Threaded server")
  (make-tls-context function)
  (create-https-server function)
  (@server-classes section)
  (@server-actions section))
