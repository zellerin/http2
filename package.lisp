;;;; Copyright 2022 by Tomáš Zellerin

;;;; package.lisp

(cl:defpackage #:http2
  (:use #:cl)
  (:export #:logging-object
           #:*do-print-log*

           #:vanilla-client-connection
           #:vanilla-server-connection
           #:get-path
           #:get-body
           #:get-headers

           #:with-http-connection
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
