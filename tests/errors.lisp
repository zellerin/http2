(in-package #:http2)

(defun test-bad-headers (headers)
  (fiasco:signals http-stream-error
      (with-http2-connection
                       (connection
                        'vanilla-client-connection
                        :network-stream (connect-to-tls-server *server-domain* :port *server-port*))
                     (loop
                       with stream = (open-http2-stream connection headers)
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

(defun send-bad-stream-id ()
  "Send request with bad stream ID. Should raise a protocol error."
  (with-http2-connection
      (connection
       'vanilla-client-connection
       :network-stream (connect-to-tls-server *server-domain* :port *server-port*)
       :id-to-use 2)
    (open-http2-stream connection
                       '((:method "HEAD")
                         (:path "/")
                         (:scheme "https")
                         (:authority "localhost"))
                       :end-stream t)
    (process-pending-frames connection)))
;; test:
;; (let ((*server-domain* "www.example.com")(*server-port* 443)) (send-bad-stream-id))

(fiasco:deftest test-bad-stream-id ()
  (fiasco:is (eq '+protocol-error+
                (get-error-code
                 (fiasco:signals go-away
                   (send-bad-stream-id))))))
