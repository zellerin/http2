;;;; Copyright 2022 by Tomáš Zellerin

(load "~/quicklisp/setup")
(asdf::load-asd (truename "./http2.asd"))
(require :sb-cover)
(ql:quickload "fiasco")
(ql:quickload "cl+ssl")
(ql:quickload "puri")
(declaim (optimize sb-cover:store-coverage-data))
(print (asdf:initialize-output-translations
        `(:output-translations
          (,(merge-pathnames "**/*.*" (asdf:system-source-directory "http2")) #p"/tmp/fasl/pre-commit-cache/**/*.*")
          :inherit-configuration :enable-user-cache)))
(ql:quickload "http2/all")

(http2::do-test)
;(load "tests/client-server-test")
;(http2::test-client-server)

(sb-cover:report (merge-pathnames
                  "cover-report/" (asdf:system-source-directory "http2")))
