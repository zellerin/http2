;;;; http2.asd

(defsystem "http2"
  :description "HTTP2 protocol implementation"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.99"
  :serial t
  :depends-on ("trivial-gray-streams" "flexi-streams")
  :components ((:file "package")
               (:module "core"
                :components ((:file "utils")
                             (:file "frames")
                             (:file "classes")
                             (:file "headers")
                             (:file "payload-streams"))))
  :in-order-to ((test-op (test-op "http2/test"))))

(defsystem "http2/tls"
  :description "Glue to wrap HTTP/2 client or server with TLS"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.9"
  :serial t
  :pathname "tls"
  :depends-on ("cl+ssl" "http2")
  :components ((:file "server")
               (:file "cl+ssl"))
  ;:in-order-to ((test-op (test-op "http2/test")))
  )

(defsystem "http2/client"
  :description "An example of http/2 client"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.3"
  :serial t
  :pathname "client"
  :depends-on ("cl+ssl" "puri" "http2")
  :components ((:file "client-utils")
               (:FILE "client")))

(defsystem "http2/server"
  :description "An example of http/2 server"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.3"
  :serial t
  :pathname "server"
  :depends-on ("puri" "http2/tls" "cl-who")
  :components ((:file "dispatch")
               (:file "server"))
  :entry-point "http2/server::main")

(defsystem "http2/test"
  :depends-on ("http2" "fiasco" "trivial-gray-streams"
                       "http2/server" "http2/client"
                       "bordeaux-threads")
  :perform (test-op (o s)
                    (symbol-call :fiasco '#:run-package-tests :package '#:http2))
  :serial t
  :license  "MIT"
  :pathname "tests"
  :components ((:file "pipe")
               (:file "test")
               (:file "tests-hpack")
               (:file "client-server-test")
               (:file "threaded-tests")
               (:file "errors")))

(defsystem "http2/all"
  :depends-on (http2 http2/test http2/client http2/server http2/tls))
