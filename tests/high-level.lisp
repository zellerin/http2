(cl:in-package :http2/tests)

(defpackage #:http2/tests/hl
  (:use #:cl #:fiasco #:http2/server #:http2/client))

(in-package #:http2/tests/hl)

(fiasco:defsuite
    (http2/tests/hl :bind-to-package #:http2/tests/hl
                 :in http2/tests::http2/tests))

(deftest client-server-match ()
  "Run server, fetch a response from it."
  (multiple-value-bind (response code)
      (multiple-value-bind (thread url)
          (start 0)
        (unwind-protect
             (retrieve-url url)
          (bordeaux-threads:destroy-thread thread)))
    (is (= code 404))
    (is (search "Not found" response))))
