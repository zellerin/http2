(cl:in-package :http2/tests)

(cl:defpackage #:http2/tests/utils
  (:use #:cl #:fiasco #:http2/utils))

(in-package #:http2/tests/utils)

(defsuite
    (http2/tests/utils :bind-to-package #:http2/tests/utils
                 :in http2/tests::http2/tests))

(deftest aref/wide/test ()
  (let ((seq #(1 2 3 4 5 6)))
    (is (= 1 (aref/wide seq 0 1)))
    (is (= #x102) (aref/wide seq 0 2))
    (is (= #x102030) (aref/wide seq 0 4))))
