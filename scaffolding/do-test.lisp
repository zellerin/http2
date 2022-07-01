;;;; Copyright 2022 by Tomáš Zellerin

(load "~/quicklisp/setup")
(asdf::load-asd (truename "./http2.asd"))
(ql:quickload "http2/all")
(load "tests/client-server-test")

(fiasco::run-package-tests :package '#:http2 )
