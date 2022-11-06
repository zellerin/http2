;;;; Copyright 2022 by Tomáš Zellerin

(load "~/quicklisp/setup")
(asdf::load-asd (truename "./http2.asd"))
(ql:quickload "http2/server")

(setf ql:*local-project-directories* (list (truename "./")))
(setf asdf:*system-definition-search-functions*
      (list 'ql::local-projects-searcher
            'ql::system-definition-searcher))
(ql:register-local-projects)
#+sbcl (require :sb-cover)
(ql:quickload "fiasco")
(ql:quickload "cl+ssl")
(ql:quickload "puri")
(ql:quickload "bordeaux-threads")
(ql:quickload "cl-who")
(ql:quickload "gzip-stream")
#+sbcl (declaim (optimize sb-cover:store-coverage-data))
#+sbcl (asdf:initialize-output-translations
         `(:output-translations
           (,(merge-pathnames "**/*.*" (asdf:system-source-directory "http2"))
             #p"/tmp/fasl/pre-commit-cache/**/*.*")
           :inherit-configuration :enable-user-cache))
(ql:quickload "http2/all")

(in-package http2)

(setf *dispatch-fn* #'funcall)
(unwind-protect
     (fiasco::run-package-tests :package '#:http2 )

  #+sbcl (handler-bind ((warning #'muffle-warning))
           (sb-cover:report (merge-pathnames
                             "cover-report/" (asdf:system-source-directory "http2")))))
