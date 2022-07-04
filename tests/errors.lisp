(in-package #:http2)

(defun test-bad-headers (headers)
  (fiasco:signals http-stream-error
      (with-http2-connection
                       (connection
                        'vanilla-client-io-connection
                        :network-stream (tls-connection-to "localhost" :port 1230))
                     (loop
                       with stream = (send-headers connection :new headers)
                       do
                          ;; if it does not signal eventually we lose.
                          (read-frame connection)))))

(fiasco:deftest empty-headers ()
  (test-bad-headers nil))

(fiasco:deftest string-headers-before-authority ()
  (test-bad-headers '((:path "/") ("foo" "bar")
                      (:scheme "https")
                      (:authority "localhost"))))
