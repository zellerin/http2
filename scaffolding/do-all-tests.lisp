;;;; Copyright 2022 by Tomáš Zellerin

(load "~/quicklisp/setup")
(asdf::load-asd (truename "./http2.asd"))
(ql:quickload "http2/all")
(load "tests/client-server-test")

(defvar *server-running* nil)
(sb-thread:make-thread (lambda ()
                         (handler-bind ((warning 'muffle-warning))
                           (http2/server:create-server 1230 "/tmp/server.key" "/tmp/server.crt"
                                                 :announce-open-fn (lambda ()
                                                                     (setf *server-running* t))))))

(wait-for *server-running* :timeout 5)
(in-package http2)
(fiasco:deftest test-client-server2 ()
  (test-webs '(("https://localhost:1230/foo" "Not found" "404")
                      ("https://localhost:1230/ok" "OK" "200"))))

(fiasco:deftest test-post ()
  (fiasco:is (equal "AB" (http2/client:retrieve-url "https://localhost:1230/body" :method "POST" :content #(65 66)))))

(fiasco:deftest test-ping ()
  (flet ((test-ping-returns (&rest pars)
           (fiasco:is (search "Ping time:"
                              (with-output-to-string (*standard-output*)
                                (apply #'http2/client:retrieve-url pars))))))
    (test-ping-returns "https://www.seznam.cz" :ping t)
    (test-ping-returns "https://localhost:1230/ok" :ping 3)
    (test-ping-returns "https://localhost:1230/ok" :ping t)))

(fiasco::run-package-tests :package '#:http2 )
