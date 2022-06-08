(load "~/quicklisp/setup")
(load "./http2.asd")
(ql:quickload "http2/test")
(http2::do-test)

(ql:quickload "http2/client")
(ql:quickload "http2/server")
(load "tests/client-server-test")

#+nil(http2::test-webs)

(http2::test-client-server)
