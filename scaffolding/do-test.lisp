(load "~/quicklisp/setup")
(load "~/projects/http2/http2.asd")
(ql:quickload "http2/test")
(http2::do-test)
