;;;; Copyright 2022 by Tomáš Zellerin

(load "~/quicklisp/setup")
(setf ql:*local-project-directories* (list (truename "./")))
(setf asdf:*system-definition-search-functions*
      (list 'ql::local-projects-searcher
            'ql::system-definition-searcher))

(asdf:load-asd (truename "./http2.asd"))
(asdf:load-system "mgl-pax")
(asdf:load-system "http2")
(setf http2/server:*logged-events* http2/server:+log-nothing+
      http2/server::*no-client-poll-timeout* 1
      http2/server::*poll-timeout* 1)
(asdf:test-system "http2")
