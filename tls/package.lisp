;; FIXME: this duplicates top-level package.lisp
(mgl-pax:define-package #:http2/openssl
    (:use #:cl #:cffi))

(mgl-pax:define-package #:http2/cl+ssl
  (:use #:cl #:http2/core #:cl+ssl #:mgl-pax #:http2/openssl))
