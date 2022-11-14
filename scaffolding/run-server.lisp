;;;; Copyright 2022 by Tomáš Zellerin

;;; this file was tested to run with sbcl, clisp, ecl

(load "~/quicklisp/setup")
(asdf::load-asd (truename "./http2.asd"))
(ql:quickload "http2/server/example")

(in-package :http2/server-example)

(run-demo-server)
