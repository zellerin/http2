(in-package #:http2/core)

(fiasco:defsuite
  (fiasco-suites::http2/core :bind-to-package #:http2/core
                             :in http2/tests::http2/tests))

(defsection @test-errors (:title "Test response to errors")
  "We test response of a server to intentionally incorrect requests."
  )

(defun run-server-with-payload (dispatcher payload-fn &rest keys)
  (multiple-value-bind (server url)
      (apply #'http2/server:start 0 :dispatcher dispatcher keys)
    (unwind-protect
         (funcall payload-fn url)
      (http2/server:stop server))))

(defun connect-to-test-server (url)
  (http2/client:connect-to-tls-server (puri:uri-host url)
                                      :port (puri:uri-port url)))

(defun test-bad-headers (headers)
  "As a client, send (presumably incorrect) HEADERS to server and read the response."
  (fiasco:signals http-stream-error
    (run-server-with-payload 'http2/server:detached-poll-dispatcher
                             (lambda (url)
                               (let ((connection (make-instance
                                                  'http2/client:vanilla-client-connection
                                                  :network-stream (connect-to-test-server url))))
                                 (loop
                                   with stream = (open-http2-stream connection headers)
                                   do
                                      (write-goaway-frame connection 1 +no-error+ nil)
                                      ;; if it does not signal eventually we lose.
                                      (http2/client:process-pending-frames connection)))))))

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

(fiasco:deftest send-bad-stream-id ()
  "Send request with bad stream ID. Should raise a protocol error."
  (let ((err
          (fiasco:signals go-away
            (run-server-with-payload 'http2/server:detached-poll-dispatcher
                                     (lambda (url)
                                       (let ((connection
                                               (make-instance 'http2/client:vanilla-client-connection
                                                              :network-stream (connect-to-test-server url)
                                                              :id-to-use 2)))
                                         (open-http2-stream connection
                                                            '((:method "HEAD")
                                                              (:path "/")
                                                              (:scheme "https")
                                                              (:authority "localhost"))
                                                            :end-stream t)
                                         (http2/client:process-pending-frames connection)))))))
    (fiasco:is (equal (get-error-code err) +protocol-error+))
    (fiasco:is (equal (map 'string 'code-char (get-debug-data err)) "OUR-ID-CREATED-BY-PEER"))))

(fiasco:deftest send-too-low-stream-id/odd ()
  "Send request with bad stream ID. Should raise a protocol error."
  (let ((err
          (fiasco:signals go-away
            (run-server-with-payload 'http2/server:detached-poll-dispatcher
                                     (lambda (url)
                                       (let ((connection
                                               (make-instance 'http2/client:vanilla-client-connection
                                                              :network-stream (connect-to-test-server url)
                                                              :id-to-use 7)))
                                         (open-http2-stream connection
                                                            '((:method "HEAD")
                                                              (:path "/")
                                                              (:scheme "https")
                                                              (:authority "localhost"))
                                                            :end-stream t)

                                         (setf (get-id-to-use connection) 1)
                                         (open-http2-stream connection
                                                            '((:method "HEAD")
                                                              (:path "/")
                                                              (:scheme "https")
                                                              (:authority "localhost"))
                                                            :end-stream t)
                                         ;; prevent bad id error on our side from S7
                                         (setf (get-id-to-use connection) 9)
                                         (http2/client:process-pending-frames connection)))))))
    (fiasco:is (equal (get-error-code err) +stream-closed+))
    (fiasco:is (equal (map 'string 'code-char (get-debug-data err)) "CLOSED-STREAM"))))

#+nil
(fiasco:deftest send-second-headers-frame ()
  ;; The idea here was to send a second headers frame to get relevant connection
  ;; error; unfortunately, second header frame is actually sometimes allowed and
  ;; more complicated to handle (trailer section) and relevant checks are to be
  ;; devised.
  (let ((err
          (fiasco:signals http-stream-error-received
            (run-server-with-payload 'http2/server:detached-poll-dispatcher
                                     (lambda (url)
                                       (let ((connection
                                               (make-instance 'http2/client:vanilla-client-connection
                                                              :network-stream (connect-to-test-server url)
                                                              :id-to-use 7)))
                                         (open-http2-stream connection
                                                            '((:method "HEAD")
                                                              (:path "/")
                                                              (:scheme "https")
                                                              (:authority "localhost"))
                                                            :end-stream nil)

                                         (setf (get-id-to-use connection) 7)
                                         (open-http2-stream connection
                                                            '((:method "HEAD")
                                                              (:path "/")
                                                              (:scheme "https")
                                                              (:authority "localhost"))
                                                            :end-stream t)
                                         (http2/client:process-pending-frames connection)))))))
    (fiasco:is (equal (get-error-code err) +stream-closed+))
    (fiasco:is (equal (map 'string 'code-char (get-debug-data err)) "BAD-STREAM-STATE"))))

;; test:
;; (let ((*server-domain* "www.example.com")(*server-port* 443)) (send-bad-stream-id))
;; (let ((*server-domain* "www.example.com")(*server-port* 443)) (send-too-low-stream-id))
;; (let ((*server-domain* "www.akamai.com")(*server-port* 443)) (send-too-low-stream-id))
