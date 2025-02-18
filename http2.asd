;;;; http2.asd

(defsystem "http2/core"
  :description "HTTP2 protocol implementation"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
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
                             (:file "classes")
                             (:file "frames")
                             (:file "frames/http2-stream")
                             (:file "frames/rst-and-goaway")
                             (:file "frames/headers")
                             (:file "frames/data")
                             (:file "frames/settings")
                             (:file "frames/push-promise")
                             (:file "frames/altsvc")
                             (:file "frames/ping")
                             (:file "binary-payload")
                             (:file "gzip-decode")
                             (:file "utf8")))))

(defsystem "http2/stream-based"
  :description "HTTP2 protocol implementation"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :serial t
  :depends-on ("http2/core" "bordeaux-threads")
  :components ((:file "package")
               (:module "core"
                :components ((:file "stream-based-connections")
                             (:file "payload-streams")))))

(defsystem "http2/client"
  :description "An example of http/2 client"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :version "2.0.1"
  :serial t
  :pathname "client"
  :depends-on ("cl+ssl" "puri" "http2/stream-based")
  :components ((:file "client-utils")
               (:FILE "client")))

(defsystem "http2/server/shared"
  :description "An example of http/2 server"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :serial t
  :pathname "server"
  ;; FIXME: is /tls really needed?
  :depends-on ("puri" #+nil "http2/tls" "http2/core" "http2/stream-based" "http2/openssl")
  :components ((:file "socket-dispatcher")
               (:file "dispatch")
               (:file "scheduler")))

(defsystem "http2/server/threaded"
  :description "An example of http/2 server"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :serial t
  :pathname "server"
  :depends-on ("puri" "http2/server/shared")
  :components ((:file "../tls/server")
               (:file "threaded")))


(defsystem "http2"
  :version "2.0.1"
  :depends-on ("http2/client" "http2/server" "http2/server/poll")
  :components ((:file "overview"))
  :description "Otherwise empty system that depends on - and thus loads when loaded - both HTTP/2 client and HTTP/2 server.

The system also supports TEST-OP to run the test suite."
  :properties ((:readme-section (@overview)))
  :in-order-to ((test-op (test-op "http2/test"))))

(defsystem "http2/test-samples"
  :depends-on ("http2/client")
  :description "Provide some test samples for common error generating patterns.
Run these patterns against servers."
  :components
  ((:file "client/payload-tests")))

(defsystem "http2/test"
  :version "0.1"
  :depends-on ("http2" "fiasco")
  :pathname "tests"
  :perform (test-op (o s)
                    (symbol-call :fiasco '#:run-package-tests :package '#:http2/tests))
  :components ((:file "tests")
               (:file "utils")
               (:file "test-samples")
               (:file "errors-lowlevel")
               (:file "high-level")
               (:file "tests-hpack")))

(defsystem "http2/openssl"
  :version "0.1"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi" "mgl-pax" "anaphora" "cl+ssl")
  :pathname "tls"
  :perform (test-op (o s)
                    (symbol-call :fiasco '#:run-package-tests :package '#:http2/tests))
  :components ((:file "../package") (:cffi-grovel-file "openssl-grovel")
               (:file "openssl")))

(asdf:defsystem "http2/server/poll"
  :description "Asyncronous polling implementations of HTTP2 server."
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :serial t
  :depends-on ("mgl-pax" "puri" "http2/server/shared" "http2/openssl")
  :pathname "server"
  :components ((:cffi-grovel-file "poll-grovel")
               (:file "poll-openssl")
               (:file "poll-server")))

(asdf:defsystem "http2/server"
  :description "Asyncronous polling implementations of HTTP2 server."
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :version "2.0.1"
  :serial t
  :depends-on ("http2/server/threaded" "http2/server/poll")
  :pathname "server"
  :components ((:cffi-grovel-file "poll-grovel")
               (:file "poll-openssl")
               (:file "poll-server")))
