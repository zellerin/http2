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
                             (:file "buffer-pool")
                             (:file "pipe")
                             (:file "errors")
                             (:file "hpack")
#+nil                             (:file "glossary")
                             (:file "frames/http2-stream")
                             (:file "classes")
                             (:file "frames")
                             (:file "frames/rst-and-goaway")
                             (:file "frames/headers")
                             (:file "frames/data")
                             (:file "frames/settings")
                             (:file "frames/push-promise")
                             (:file "frames/altsvc")
                             (:file "frames/ping")
                             (:file "gzip-decode")
                             (:file "utf8")))))

(defsystem "http2/stream-based"
  :description "HTTP2 protocol implementation"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :serial t
  :depends-on ("http2/core" "bordeaux-threads")
  :pathname "core"
  :components ((:file "stream-based-connections")
                 (:file "payload-streams")))

(defsystem "http2/client"
  :description "An example of http/2 client"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :serial t
  :pathname "client"
  :depends-on ("cl+ssl" "puri" "http2/stream-based")
  :components ((:file "client-utils")
               (:file "client")
               (:file "tutorials")))

(defsystem "http2/server/shared"
  :description "An example of http/2 server"
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :license  "MIT"
  :serial t
  :pathname "server"
  ;; FIXME: is /tls really needed?
  :depends-on ("puri" #+nil "http2/tls" "http2/core" "http2/stream-based" "http2/openssl")
  :components ((:file "socket-dispatcher")
               (:file "logging")
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
  :version "2.1"
  :depends-on ("http2/client" "http2/server" "http2/server/poll")
  :components ((:file "overview"))
  :description "HTTP/2 library, including a sample client and server.

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
  :depends-on ("http2" "fiasco")
  :pathname "tests"
  :perform (test-op (o s)
                    (symbol-call :fiasco '#:run-package-tests :package '#:http2/tests))
  :components ((:file "support")
               (:file "tests")
               (:file "tcpip")
               (:file "utils")
               (:file "test-samples")
               (:file "errors")
               (:file "errors-lowlevel")
               (:file "high-level")
               (:file "tests-hpack")
               (:file "server")
               (:file "poll-server")
               (:file "frames")
               (:file "frames/headers")
               (:file "frames/data")
               (:file "test")))

(defsystem "http2/tcpip"
  ;; note: it has to depend on cl+ssl
  :depends-on ("cffi" "mgl-pax" "anaphora" "cl+ssl")
  :defsystem-depends-on ("cffi-grovel")
  :components ((:file "package")
               (:cffi-grovel-file "server/poll-grovel")
               (:file "server/tcpip")))

(defsystem "http2/openssl"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi" "mgl-pax" "anaphora" "http2/tcpip" "http2/core")
  :pathname "tls"
  :components ((:cffi-grovel-file "openssl-grovel")
               (:file "openssl")))

(asdf:defsystem "http2/server/poll"
  :description "Asyncronous polling implementations of HTTP2 server."
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :serial t
  :depends-on ("mgl-pax" "puri" "http2/server/shared" "http2/openssl" "let-over-lambda")
  :pathname "server"
  :components ((:file "poll-openssl")
               (:file "poll-server")))

(asdf:defsystem "http2/server"
  :description "HTTP/2 server interface. Provides access to an implementation of a HTTP/2 server
- both for running the server and for defining content."
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :serial t
  :depends-on ("http2/server/threaded" "http2/server/poll")
  :pathname "server")

(asdf:defsystem "http2/server/demo"
  :description "Demo content for the HTTP/2 server."
  :author "Tomáš Zellerin <tomas@zellerin.cz>"
  :serial t
  :depends-on ("http2/server" "cl-who" "parenscript" "let-over-lambda")
  :pathname "server"
  :components ((:file "demo")))
