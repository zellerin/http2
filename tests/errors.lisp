(in-package #:http2)

(defun test-bad-headers (headers)
  (fiasco:signals http-stream-error
      (with-http2-connection
                       (connection
                        'vanilla-client-connection
                        :network-stream (connect-to-tls-server "localhost" :port *server-port*))
                     (loop
                       with stream = (send-headers connection headers)
                       do
                          ;; if it does not signal eventually we lose.
                          (read-frame connection)))))

(fiasco:deftest empty-headers ()
  (test-bad-headers nil))

(fiasco:deftest string-headers-before-authority ()
  (test-bad-headers '((:path "/") ("foo" "bar")
                      (:scheme "https")
                      (:authority "localhost"))))

(fiasco:deftest uppercase-headers ()
  (test-bad-headers '((:method "GET")
                      (:path "/")
                      (:scheme "https")
                      (:authority "localhost")
                      ("FOO" "bar"))))
