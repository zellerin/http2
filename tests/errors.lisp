(in-package #:http2)

(defun connect-to-test-server ()
  (connect-to-tls-server (puri:uri-host *server-url*)
                         :port (puri:uri-port *server-url*)))

(defun test-bad-headers (headers)
  "As a client, send (presumably incorrect) HEADERS to server and read the response."
  (with-test-server ('tls-single-client-dispatcher)
    (fiasco:signals http-stream-error
      (with-http2-connection
          (connection
           'vanilla-client-connection
           :network-stream (connect-to-test-server))
        (loop
          with stream = (open-http2-stream connection headers)
          do
             ;; if it does not signal eventually we lose.
             (read-frame connection))))))

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

(define-protocol-error-test send-bad-stream-id +protocol-error+
  "Send request with bad stream ID. Should raise a protocol error."
  (with-test-server ('tls-single-client-dispatcher)
    (with-http2-connection
        (connection
         'vanilla-client-connection
         :network-stream (connect-to-test-server)
         :id-to-use 2)
      (open-http2-stream connection
                         '((:method "HEAD")
                           (:path "/")
                           (:scheme "https")
                           (:authority "localhost"))
                         :end-stream t)
      (process-pending-frames connection))))

(define-protocol-error-test send-too-low-stream-id/odd +stream-closed+
  "The low IDs are supposed to be closed when higher number is seen."
  (with-test-server ('tls-single-client-dispatcher)
      (with-http2-connection
          (connection
           'vanilla-client-connection
           :network-stream (connect-to-test-server)
           :id-to-use 7)
        (http-stream-to-vector
         (open-http2-stream connection
                            '((:method "HEAD")
                              (:path "/")
                              (:scheme "https")
                              (:authority "localhost"))
                            :end-stream t))
        (setf (get-id-to-use connection) 1)
        (http-stream-to-vector
         (open-http2-stream connection
                            '((:method "HEAD")
                              (:path "/")
                              (:scheme "https")
                              (:authority "localhost"))
                            :end-stream t)))))

;; test:
;; (let ((*server-domain* "www.example.com")(*server-port* 443)) (send-bad-stream-id))
;; (let ((*server-domain* "www.example.com")(*server-port* 443)) (send-too-low-stream-id))
;; (let ((*server-domain* "www.akamai.com")(*server-port* 443)) (send-too-low-stream-id))
