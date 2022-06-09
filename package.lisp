;;;; package.lisp

(cl:defpackage #:http2
  (:use #:cl)
  (:export #:client-http2-connection
           #:client-stream
           #:server-stream
           #:server-http2-connection
           #:logging-object
           #:*do-print-log*


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
           #:peer-acks-settings))
