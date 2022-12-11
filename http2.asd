;;;; http2.asd

(defsystem "http2"
  :description "HTTP2 protocol implementation"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "1.0"
  :serial t
  :depends-on ("trivial-gray-streams" "flexi-streams"
                                      "anaphora"  "gzip-stream" "alexandria")
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
  :version "1.0"
  :serial t
  :pathname "tls"
  :depends-on ("cl+ssl" "http2" "bordeaux-threads")
  :components ((:file "server")
               (:file "cl+ssl"))
  ;:in-order-to ((test-op (test-op "http2/test")))
  )

(defsystem "http2/client"
  :description "An example of http/2 client"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.4"
  :serial t
  :pathname "client"
  :depends-on ("cl+ssl" "puri" "http2")
  :components ((:file "client-utils")
               (:FILE "client")))

(defsystem "http2/server"
  :description "An example of http/2 server"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.4"
  :serial t
  :pathname "server"
  :depends-on ("puri" "http2/tls")
  :components ((:file "dispatch")
               (:file "scheduler")))

(defsystem "http2/server/example"
  :description "An example of http/2 server."
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.4"
  :serial t
  :pathname "server"
  :depends-on ("puri" "http2/tls" "cl-who"
                      "parenscript"
                      "http2/server")
  :components ((:file "dispatch")
               (:file "scheduler")
               (:file "server"))
  :entry-point "http2/server-example::run-demo-server")

(defsystem "http2/test"
  :depends-on ("http2" "fiasco" "trivial-gray-streams"
                       "http2/server" "http2/client" "http2/server/example"
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
               (:file "errors")
               (:file "streams")))

(defsystem "http2/all"
  :depends-on (http2 http2/test http2/client http2/server http2/server/example http2/tls))
