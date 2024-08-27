;;;; Copyright 2022 by Tomáš Zellerin

(load "~/quicklisp/setup")
(setf ql:*local-project-directories* (list (truename "./")))
(setf asdf:*system-definition-search-functions*
      (list 'ql::local-projects-searcher
            'ql::system-definition-searcher))

#+sbcl (require :sb-cover)
(ql:quickload "fiasco")
(ql:quickload "cl+ssl")
(ql:quickload "puri")
(ql:quickload "bordeaux-threads")
(ql:quickload "cl-who")
(ql:quickload "gzip-stream")
(ql:quickload "mgl-pax")
(map nil 'ql:quickload '("chipz" "cl-ppcre"))


(asdf::load-asd (truename "./http2.asd"))

#+sbcl (declaim (optimize sb-cover:store-coverage-data))
#+sbcl (asdf:initialize-output-translations
         `(:output-translations
           (,(merge-pathnames "**/*.*" (asdf:system-source-directory "http2"))
             #p"/tmp/fasl/pre-commit-cache/**/*.*")
           :inherit-configuration :enable-user-cache))

(asdf:load-system :http2/all :force t)

(in-package http2)
(http2/server-example::maybe-create-certificate "certs/server.key" "certs/server.crt" :system "http2")

(setf *dispatch-fn* #'funcall)
(unwind-protect
     (fiasco::run-package-tests :package '#:http2 )

  #+sbcl (handler-bind ((warning #'muffle-warning))
           (sb-cover:report (merge-pathnames
                             "cover-report/" (asdf:system-source-directory "http2")))))
