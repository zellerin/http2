(load "~/quicklisp/setup")
(load "./http2.asd")
(ql:quickload "http2/test")
(http2::do-test)

(ql:quickload "http2/client")
(ql:quickload "http2/server")
(load "tests/client-server-test")

(http2::test-webs)

(http2::test-client-server)

(sb-thread:make-thread (lambda ()
                         (handler-bind ((warning 'muffle-warning))
                           (http2::create-server 1230 "/tmp/server.key" "/tmp/server.crt"
                                          :verbose nil))))

(sleep 1) ;; fixme: have server announce it is ready or check otherwise
(http2::test-webs '(("https://localhost:1230/foo" "Not found" "404")
                    ("https://localhost:1230/exit" "Goodbye" "200")))
