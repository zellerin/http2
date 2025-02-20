(cl:defpackage #:http2/tests/server
  (:use #:cl #:fiasco #:http2/server))

(in-package #:http2/tests/server)

(defsuite
    (http2/tests/server :bind-to-package #:http2/tests/server
                 :in http2/tests::http2/tests))


(deftest test-tls-flags ()
  "Test that TLS flag is correctly set."
  (fiasco:is (http2/server::get-tls (make-instance 'tls-threaded-dispatcher)))
  (fiasco:is (null (http2/server::get-tls (make-instance 'threaded-dispatcher)))))
