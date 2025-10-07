;;;; Copyright 2022-2025 by Tomáš Zellerin

(mgl-pax:define-package #:http2/utils
    (:use :cl :mgl-pax)
  (:import-from #:anaphora #:acond #:aif #:it))

(mgl-pax:define-package :http2/hpack
  (:use :cl #:anaphora #:http2/utils)
  (:import-from #:mgl-pax #:defsection #:glossary-term #:section
                #:define-glossary-term))

(mgl-pax:define-package #:http2/openssl
  (:use #:cl #:cffi #:mgl-pax #:dref #:http2/utils)
  (:import-from #:http2/tcpip #:communication-error))

(mgl-pax:define-package :http2/core
  (:use :cl :http2/hpack :http2/utils)
  (:import-from :anaphora #:awhen #:acond #:it)
  (:import-from #:mgl-pax #:defsection #:glossary-term #:section
                #:define-glossary-term)
  (:import-from :alexandria
                #:read-stream-content-into-string #:read-stream-content-into-byte-vector))

(mgl-pax:define-package #:http2/cl+ssl
  (:use #:cl #:http2/core #:cl+ssl #:mgl-pax #:http2/openssl))

(mgl-pax:define-package #:http2/stream-overlay
    (:use #:cl #:http2/core #:http2/utils)
  (:import-from #:anaphora #:acond #:awhen #:aif #:it))

(mgl-pax:define-package #:http2/client
    (:use #:cl #:http2/core #:http2/stream-overlay #:alexandria
          #:http2/utils #:mgl-pax #:dref)
  (:import-from #:anaphora #:acond #:aif #:it)
  (:documentation "HTTP/2 client functions, in particular, RETRIEVE-URL."))

(mgl-pax:define-package #:http2/tcpip
  (:use #:cl #:mgl-pax #:cffi #:http2/utils))

(mgl-pax:define-package #:http2/server
    (:use #:cl #:http2/core #:mgl-pax #:http2/stream-overlay #:http2/utils
          #:http2/openssl #:dref #:cffi
          #:http2/tcpip)
  (:nicknames #:http2/server/shared #:http2/server/poll #:http2/server/threaded)
  (:documentation "HTTP/2 server functions - for example START to start the server and DEFINE-EXACT-HANDLER and
HANDLER macro to define content to serve."))

#+bil(mgl-pax:define-package #:http2/server/poll
    (:use #:cl #:cffi #:http2/server/shared #:http2/openssl
          #:http2/core))

#+bil(mgl-pax:define-package #:http2/server/threaded
  (:use #:cl #:cl+ssl #:http2/cl+ssl #:http2/server/shared #:http2/server/poll
        #:http2/openssl
        #:http2/core))

#+nil(mgl-pax:define-package #:http2/server
    (:use #:cl #:cffi #:http2/server/shared #:http2/server/poll #:http2/server/threaded
          #:http2/core)
  (:export #:start #:kill-server #:define-exact-handler #:handler #:http-stream-to-string)
  (:nicknames #:http2/server/cffi))

(mgl-pax:define-package #:http2
    (:use #:cl #:mgl-pax #:http2/server #:http2/client))
