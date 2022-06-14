;;;; Copyright 2022 by Tomáš Zellerin

(load "~/quicklisp/setup")
(load "./http2.asd")
(ql:quickload "http2/all")
(http2::do-test)
(load "tests/client-server-test")
(http2::test-client-server)
