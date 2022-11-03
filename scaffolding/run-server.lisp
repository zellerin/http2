;;;; Copyright 2022 by Tomáš Zellerin

;;; this file was tested to run with sbcl, clisp, ecl

(load "~/quicklisp/setup")
(asdf::load-asd (truename "./http2.asd"))
(ql:quickload "http2/server")

(in-package :http2/server)

(unless (and (probe-file "/tmp/server.key")
             (probe-file "/tmp/server.crt"))
  (format t "~%Generating temporary certificates")
  (uiop:run-program "openssl req -new -nodes -x509 -days 365 -subj /CN=localhost -keyout /tmp/server.key -outform PEM -out /tmp/server.crt")
  (terpri))

(handler-bind ((warning 'muffle-warning)
               (error (lambda (e)
                        (describe e)
                        (invoke-restart 'kill-server))))
  (create-https-server 1230 "/tmp/server.key" "/tmp/server.crt"
                 :verbose nil))
