;;;; Copyright 2022 by Tomáš Zellerin

(load "~/quicklisp/setup")
(setf ql:*local-project-directories* (list (truename "./")))
(setf asdf:*system-definition-search-functions*
      (list 'ql::local-projects-searcher
            'ql::system-definition-searcher))

(asdf:load-asd (truename "./http2.asd"))

(asdf:test-system :http2)
