;;;; http2.asd
(in-package :asdf-user)

(asdf:defsystem #:http2
  :description "HTTP2 protocol implementation"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.2"
  :serial t
  :components ((:file "package")
               (:module "core"
                :components ((:file "utils")
                             (:file "frames")
                             (:file "classes")
                             (:file "headers"))))
  :in-order-to ((test-op (test-op "http2/test"))))

(asdf:defsystem #:http2/client
  :description "HTTP2 protocol implementation"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.2"
  :serial t
  :pathname "client"
  :depends-on (#:cl+ssl #:puri #:http2)
  :components ((:file "client-utils")
               (:FILE "client")))

(asdf:defsystem #:http2/server
  :description "HTTP2 protocol implementation"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.2"
  :serial t
  :pathname "server"
  :depends-on (#:cl+ssl #:puri #:http2)
  :components ((:file "cl+ssl")
               (:file "dispatch")
               (:file "server"))
  :entry-point "http2/server::main")

(asdf:defsystem #:http2/test
  :depends-on (http2 fiasco trivial-gray-streams)
  :perform (test-op (o s)
                    (uiop:symbol-call :http2 'do-test))
  :serial t
  :license  "MIT"
  :pathname "tests"
  :components ((:file "pipe")
               (:file "test")
               (:file "tests-hpack")))

(asdf:defsystem #:http2/all
  :depends-on (http2 http2/test http2/client http2/server))
