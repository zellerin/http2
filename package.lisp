;;;; Copyright 2022-2024 by Tomáš Zellerin

;;;; package.lisp

(mgl-pax:define-package #:http2/utils
    (:use :cl :mgl-pax)
  (:import-from #:anaphora #:acond #:aif #:it))

(mgl-pax:define-package :http2/hpack
  (:use :cl #:anaphora #:http2/utils)
  (:import-from #:mgl-pax #:defsection #:glossary-term #:section
                #:define-glossary-term))

(mgl-pax:define-package :http2-core
    (:nicknames #:http2/core)
  (:use :cl :http2/hpack :http2/utils)
  (:import-from :anaphora #:awhen #:acond #:it)
  (:import-from #:mgl-pax #:defsection #:glossary-term #:section
                #:define-glossary-term)
  (:import-from :alexandria
                #:read-stream-content-into-string #:read-stream-content-into-byte-vector))

(mgl-pax:define-package #:http2/stream-overlay
    (:use #:cl #:http2/core #:http2/utils)
  (:import-from #:anaphora #:acond #:awhen #:aif #:it))

(mgl-pax:define-package #:http2/cl+ssl
  (:use #:cl #:http2/core #:cl+ssl #:mgl-pax))

(mgl-pax:define-package #:http2/client
    (:use #:cl #:http2/core #:http2/stream-overlay #:alexandria
          #:http2/utils)
  (:import-from #:anaphora #:acond #:aif #:it)
  (:documentation "HTTP/2 client functions, in particular, RETRIEVE-URL."))

; FIXME: 2024-12-26 this causes http2 package depend on cl+ssl, not ideal.
; Fix this and then remove dependency.
(mgl-pax:define-package #:http2-server
    (:nicknames #:http2/server)
  (:use #:cl #:http2/core #:mgl-pax #:http2/stream-overlay #:http2/utils
        #:http2/cl+ssl)
  (:documentation "HTTP/2 server functions - for example START to start the server and DEFINE-EXACT-HANDLER and
HANDLER to define content to serve."))

(mgl-pax:define-package #:http2
  (:use #:cl #:mgl-pax #:http2/server #:http2/client))
