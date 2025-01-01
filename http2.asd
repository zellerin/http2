;;;; http2.asd

(defsystem "http2/core"
  :description "HTTP2 protocol implementation"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "1.1"
  :serial t
  :depends-on ("trivial-gray-streams" "flexi-streams"
                                      "anaphora" "gzip-stream" "alexandria"
                                      "trivial-utf-8"
                                      "chipz"
                                      "mgl-pax"
                                      "cl+ssl")
  :components ((:file "package")
               (:module "core"
                :components ((:file "utils")
                             (:file "pipe")
                             (:file "errors")
                             (:file "hpack")
#+nil                             (:file "glossary")
                             (:file "frames")
                             (:file "frames/http2-stream")
                             (:file "frames/rst-and-goaway")
                             (:file "frames/headers")
                             (:file "frames/data")
                             (:file "frames/settings")
                             (:file "frames/push-promise")
                             (:file "frames/altsvc")
                             (:file "frames/ping")
                             (:file "classes")
                             (:file "binary-payload")
                             (:file "gzip-decode")
                             (:file "utf8"))))
  :in-order-to ((test-op (test-op "http2/test"))))

(defsystem "http2/stream-based"
  :description "HTTP2 protocol implementation"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "1.1"
  :serial t
  :depends-on ("http2/core" "bordeaux-threads")
  :components ((:file "package")
               (:module "core"
                :components ((:file "stream-based-connections")
                             (:file "payload-streams"))))
  :in-order-to ((test-op (test-op "http2/test"))))

(defsystem "http2/tls"
  :description "Glue to wrap HTTP/2 client or server with TLS"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "1.0"
  :serial t
  :pathname "tls"
  :depends-on ("cl+ssl" "http2/stream-based" "bordeaux-threads")
  :components ((:file "cl+ssl")
               (:file "server"))
                                        ;:in-order-to ((test-op (test-op "http2/test")))
  )

(defsystem "http2/client"
  :description "An example of http/2 client"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.4"
  :serial t
  :pathname "client"
  :depends-on ("cl+ssl" "puri" "http2/stream-based")
  :components ((:file "client-utils")
               (:FILE "client")))

(defsystem "http2/server"
  :description "An example of http/2 server"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "0.5"
  :serial t
  :pathname "server"
  ;; FIXME: is /tls really needed?
  :depends-on ("puri" "http2/tls")
  :components ((:file "socket-dispatcher")
               (:file "scheduler")
               (:file "dispatch")))

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
#+nil               (:file "server"))
  :entry-point "http2/server-example::run-demo-server")

(defsystem "http2/test"
  :depends-on ("http2/core" "fiasco" "trivial-gray-streams"
                       "http2/server" "http2/client" "http2/server/example"
                       "bordeaux-threads")
  :perform (test-op (o s)
                    (symbol-call :fiasco '#:run-package-tests :package '#:http2))
  :serial t
  :license  "MIT"
  :pathname "tests"
  :components ((:file "pipe")
               (:file "test")
               (:file "frames")
               (:file "tests-hpack")
               (:file "errors-lowlevel")
               (:file "threaded-tests")
               (:file "errors")
               (:file "streams")))

(defsystem "http2"
  :depends-on (#+nil http2/test "http2/client" "http2/server")
  :components ((:file "overview"))
  :description "Load this system to load all HTTP/2 components - in particular, both client and
server.")
