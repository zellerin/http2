;;;; Copyright 2022 by Tomáš Zellerin

(load "~/quicklisp/setup")
(require :sb-cover)
(ql:quickload "fiasco")
(ql:quickload "cl+ssl")
(ql:quickload "puri")
(declaim (optimize sb-cover:store-coverage-data))
(load "./http2.asd")
(ql:quickload "http2" :force t)
(ql:quickload "http2/all" :force t)

(http2::do-test)
(load "tests/client-server-test")
#+nil(http2::test-webs)
#+nil(http2::test-client-server)

(defvar *server-running* nil)
(sb-thread:make-thread (lambda ()
                         (handler-bind ((warning 'muffle-warning))
                           (http2/server::create-server 1230 "/tmp/server.key" "/tmp/server.crt"
                                                 :announce-open-fn (lambda ()
                                                                     (setf *server-running* t))))))

(wait-for *server-running* :timeout 5)

(http2::test-webs '(("https://localhost:1230/foo" "Not found" "404")
                    ("https://localhost:1230/ok" "OK" "200")))

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

(test-post)
(test-ping)

;;; Produce a coverage report
(sb-cover:report (merge-pathnames
                  "cover-report/" (asdf::component-pathname (asdf:find-system "http2"))))

;;; Turn off instrumentation
(declaim (optimize (sb-cover:store-coverage-data 0)))
