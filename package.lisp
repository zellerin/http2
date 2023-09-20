;;;; Copyright 2022 by Tomáš Zellerin

;;;; package.lisp

(mgl-pax:define-package :http2/hpack
  (:use :cl #:anaphora)
  (:import-from #:mgl-pax #:defsection #:glossary-term #:section
                #:define-glossary-term)
  (:export #:compile-headers #:decode-huffman
           #:dynamic-table-value
           #:get-bytes-left-in-table #:get-deleted-items #:get-dynamic-table #:get-dynamic-table-size #:get-integer-from-octet #:get-updates-needed #:integer-to-array #:read-http-header #:hpack-context
           ;; used by it
           #:read-byte*
           ;; reexported from http2
           #:encode-header #:request-headers #:update-dynamic-table-size))

(mgl-pax:define-package :http2
  (:use :cl :http2/hpack)
  (:import-from :anaphora #:awhen #:acond #:it)
  (:import-from #:mgl-pax #:defsection #:glossary-term #:section
                #:define-glossary-term)
  (:import-from :alexandria
                #:read-stream-content-into-string #:read-stream-content-into-byte-vector)
  (:export #:logging-object
           #:*do-print-log*

           #:connect-to-tls-server
           #:vanilla-client-connection
           #:vanilla-client-io-connection
           #:vanilla-client-stream
           #:vanilla-server-connection
           #:vanilla-server-stream
           #:header-collecting-mixin
           #:get-path
           #:get-headers
           #:encode-header
           #:http-stream-to-vector

           #:connection #:stream #:server-stream
           #:with-http-connection ; obsolete
           #:with-http2-connection
           #:get-finished
           #:send-headers #:add-header
           #:send-ping
           #:send-payload
           #:wait-for-responses
           #:terminate-locally

           #:request-headers ; compile headers for a request

           #:peer-opens-http-stream
           #:peer-ends-http-stream
           #:peer-sends-push-promise
           #:peer-resets-stream
           #:peer-pushes-promise
           #:apply-data-frame
           #:apply-window-size-increment
           #:apply-stream-priority
           #:update-dynamic-table-size
           #:set-peer-setting
           #:peer-expects-settings-ack
           #:peer-acks-settings

           #:extract-charset-from-content-type
           #:make-transport-stream
           #:make-transport-output-stream
           #:binary-output-stream-over-data-frames
           ;; Server
           #:create-https-server
           #:*dispatch-fn*
           #:define-prefix-handler
           #:define-exact-handler
           #:handler #:constant-handler
           #:scheduling-handler
           #:schedule-task
           #:get-lock #:get-scheduler
           #:redirect-handler
           #:send-text-handler
           #:send-text
           #:send-headers
           #:send-goaway
           #:process-server-stream
           #:get-body
           #:kill-server #:kill-connection
           #:get-header
           #:get-connection
           #:get-status

           #:+no-error+

           #:DO-PONG
           #:WRITE-DATA-FRAME #:WRITE-ALTSVC-FRAME
           #:WRITE-PING-FRAME #:WRITE-PRIORITY-FRAME
           #:CREATE-ONE-SHOT-SERVER #:DISPATCHER-MIXIN
           #:READ-FRAME #:WRITE-FRAME-HEADER
           #:CLIENT-HTTP2-CONNECTION #:WRITE-HEADERS-FRAME
           #:WRITE-SETTINGS-FRAME
           #:WRAP-TO-TLS-AND-PROCESS-SERVER-STREAM
           #:WRITE-PUSH-PROMISE-FRAME
           #:WRITE-RST-STREAM-FRAME #:HISTORY-PRINTING-OBJECT
           #:CLIENT-STREAM #:TIMESHIFT-PINGING-CONNECTION
           #:WRITE-GOAWAY-FRAME #:WRITE-ACK-SETTING-FRAME
           #:HANDLE-UNDEFINED-FRAME #:WRITE-WINDOW-UPDATE-FRAME
           #:DO-GOAWAY #:WRITE-CONTINUATION-FRAME))
