;;;; Copyright 2022 by Tomáš Zellerin

;;;; package.lisp

(cl:defpackage :http2
  (:use :cl)
  (:export #:logging-object
           #:*do-print-log*
           #:*use-huffman-coding-by-default*

           #:vanilla-client-connection
           #:vanilla-client-io-connection
           #:vanilla-client-stream
           #:vanilla-server-connection
           #:vanilla-server-stream
           #:header-collecting-mixin
           #:get-path
           #:get-body
           #:get-headers
           #:encode-header

           #:connection #:stream
           #:with-http-connection ; obsolete
           #:with-http2-connection
           #:get-finished
           #:send-headers
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

           #:binary-output-stream-over-data-frames
           ;; Server
           #:create-https-server
           #:*dispatch-fn*
           #:define-prefix-handler
           #:define-exact-handler
           #:handler
           #:redirect-handler
           #:send-text-handler
           #:send-text
           #:send-headers
           #:send-goaway
           #:process-server-stream
           #:get-body

           #:+no-error+))
