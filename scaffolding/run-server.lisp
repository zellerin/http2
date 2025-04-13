;;;; Copyright 2022, 2025 by Tomáš Zellerin

;;; this file was tested to run with sbcl
;;; TODO: fix ecl at least
;;; ecl --load


(cl:load "~/quicklisp/setup")
(require 'asdf)
(asdf:load-asd (truename "./http2.asd"))
(asdf:load-system "http2/server/demo")

(in-package http2/demo)

(start 8081 :dispatcher 'detached-poll-dispatcher)
(start 8080 :dispatcher 'detached-tls-threaded-dispatcher)
(do () (nil) (sleep 100))
