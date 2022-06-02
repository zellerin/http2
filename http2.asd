;;;; http2.asd
(in-package :asdf-user)

(asdf:defsystem #:http2
  :description "HTTP2 protocol implementation"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.2"
  :serial t
  :depends-on (#:cl+ssl #:puri)
  :components ((:file "package")
               (:file "utils")
               (:file "http2")
               (:file "classes")
               (:file "headers")
               (:file "client-utils")
               (:FILE "client"))
  :in-order-to ((test-op (test-op "http2/test"))))

(asdf:defsystem #:http2/test
  :depends-on (http2 stefil trivial-gray-streams)
  :perform (test-op (o s)
                    (uiop:symbol-call :http2 'do-test) )
  :components ((:file "pipe")
               (:file "test")))
