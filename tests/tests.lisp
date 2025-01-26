(mgl-pax:define-package #:http2/tests
  (:use #:cl #:fiasco #:http2/server #:http2/client))

(in-package :http2/tests)

(defsuite
    (http2/tests :bind-to-package #:http2/tests))
