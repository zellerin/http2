;;;; http2.asd
(in-package :asdf-user)

(asdf:defsystem #:http2
  :description "Describe http2 here"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "Specify license here"
  :version "0.1"
  :serial t
  :depends-on (#:cl+ssl #:puri)
  :components ((:file "package")
               (:file "http2")
               (:file "classes")
               (:file "headers")
               (:file "client"))
  :in-order-to ((test-op (test-op "http2/test"))))

(asdf:defsystem #:http2/test
  :depends-on ( http2 lisp-unit)
  :perform (test-op (o s)
                    (uiop:symbol-call :lisp-unit '#:run-tests :all 'http2/test) )
  :components ((:file "test")))
