;;;; Copyright 2022 by Tomáš Zellerin

(load "~/quicklisp/setup")
(require :sb-cover)
(asdf::load-asd (truename "./http2.asd"))
(ql:quickload "fiasco")
(ql:quickload "cl+ssl")
(ql:quickload "puri")
(declaim (optimize sb-cover:store-coverage-data))
(print (asdf:initialize-output-translations
        `(:output-translations
          (,(merge-pathnames "**/*.*" (asdf:system-source-directory "http2")) #p"/tmp/fasl/pre-commit-cache/**/*.*")
          :inherit-configuration :enable-user-cache)))
(ql:quickload "http2/all")
(load "tests/client-server-test")


(in-package http2)
(defvar *server-running* nil)
(sb-thread:make-thread (lambda ()
                         (handler-bind ((warning 'muffle-warning))
                           (http2:create-https-server 1230 "/tmp/server.key" "/tmp/server.crt"
                                                 :announce-open-fn (lambda ()
                                                                     (setf *server-running* t))))))

(sb-ext:wait-for *server-running* :timeout 5)

(fiasco:deftest test-self-compatible ()
  (test-webs '(("https://localhost:1230/foo" "Not found" "404")
               ("https://localhost:1230/ok" "OK" "200"))))

(defun test-curl (url content)
  (fiasco:is (search content (uiop:run-program `("/usr/bin/curl" ,url "-k") :output :string))))

(fiasco:deftest test-curl-access ()
  (test-curl "https://localhost:1230/ok" "Redirect was OK"))

(fiasco:deftest test-post ()
  (fiasco:is (equal "AB" (http2/client:retrieve-url "https://localhost:1230/body" :method "POST" :content #(65 66)))))

(fiasco:deftest test-post-2 ()
  (fiasco:is (equal "AB"
                    (http2/client:retrieve-url "https://localhost:1230/body"
                                               :method "POST"
                                               :content-fn
                                               (lambda (out) (write-sequence #(65 66) out))))))

(fiasco:deftest test-post-3 ()
  (fiasco:is (equal "AB"
                    (http2/client:retrieve-url "https://localhost:1230/body"
                                               :method "POST"
                                               :content-fn
                                               (lambda (out)
                                                 (write-sequence #(65 66) out))))))

(fiasco:deftest test-ping ()
  (flet ((test-ping-returns (&rest pars)
           (fiasco:is (search "Ping time:"
                              (with-output-to-string (*standard-output*)
                                (apply #'http2/client:retrieve-url pars))))))
    (test-ping-returns "https://localhost:1230/ok" :ping 3)
    (test-ping-returns "https://localhost:1230/ok" :ping t)))

(fiasco::run-package-tests :package '#:http2 )

(handler-bind ((warning #'muffle-warning))
  (sb-cover:report (merge-pathnames
                    "cover-report/" (asdf:system-source-directory "http2"))))
