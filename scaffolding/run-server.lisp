(load "~/quicklisp/setup")
(load "./http2.asd")
(ql:quickload "http2/server")

(in-package http2)
(handler-bind ((warning 'muffle-warning))
  (create-server 1230 "/tmp/server.key" "/tmp/server.crt"
                 :verbose nil))