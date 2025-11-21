(cl:in-package :http2/tests)

(defpackage #:http2/tests/high-level
  (:use #:cl #:fiasco #:http2/server #:http2/client))

(in-package #:http2/tests/high-level)

(fiasco:defsuite
    (http2/tests/high-level :bind-to-package #:http2/tests/high-level
                 :in http2/tests::http2/tests))

(deftest client-server-match ()
  "Run server, fetch a response from it."
  (let ((http2/server:*poll-timeout* 0.5)
        (http2/server:*no-client-poll-timeout* 0.5))
    (dolist (dispatcher-class '(http2/server::detached-poll-dispatcher http2/server::detached-tls-threaded-dispatcher))
      (multiple-value-bind (response code)
          (multiple-value-bind (dispatcher url)
              (start 0 :dispatcher dispatcher-class)
            (unwind-protect
                 (retrieve-url (puri:merge-uris "/doesnotexists" url))
              (stop dispatcher)))
        (is (= code 404))
        (is (search "Not found" response))))))

(define-exact-handler "/hello-world"
  (handler (foo :utf-8 nil)
    (with-open-stream (foo foo)
      (send-headers
       '((:status "200")
         ("content-type" "text/html; charset=utf-8")))
      (format foo "Hello World, this is random: ~a" (random 10)))))

(deftest tutorial-server-content ()
  "Run server, fetch a response from it."
  (let ((http2/server:*poll-timeout* 0.5)
        (http2/server:*no-client-poll-timeout* 0.5))
    (dolist (dispatcher-class '(http2/server::detached-poll-dispatcher http2/server::detached-tls-threaded-dispatcher))
      (multiple-value-bind (response code)
          (multiple-value-bind (dispatcher url)
              (start 0 :dispatcher dispatcher-class)
            (unwind-protect
                 (retrieve-url (puri:merge-uris "/hello-world" url))
              (stop dispatcher)))
        (is (= code 200))
        (is (search "Hello World, this is random" response))))))

(define-exact-handler "/body-and-headers"
  (handler (foo :utf-8 nil)
    (with-open-stream (foo foo)
      (send-headers
       '((:status "200")
         ("content-type" "text; charset=utf-8")))
      (format foo "~a request; ~a ~s; ~s"
         (http2/core::get-method stream)
         (http-stream-to-string stream)
         (http2/core:get-headers stream)
         (http2/core:get-body stream)))))

(deftest tutorial-client-parameters ()
  "Run server, fetch a response from it."
  (dolist (dispatcher '(detached-tls-threaded-dispatcher http2/server::detached-poll-dispatcher))
    (multiple-value-bind (dispatcher url)
        (start 0 :dispatcher dispatcher)
      (unwind-protect
           (progn
             (multiple-value-bind (response code)
                 (retrieve-url (puri:merge-uris "/body-and-headers" url)
                               :content "ABC")
               (is (= code 200))
               (is (equal response "POST request; ABC ((\"content-type\" . \"text/plain; charset=utf-8\")); NIL")))
             (multiple-value-bind (response code)
                 (retrieve-url (puri:merge-uris "/body-and-headers" url)
                               :content #(1 2 3))
               (is (= code 200))
               (is (equal response "POST request;  ((\"content-type\" . \"application/octet-stream\")); #(1 2 3)"))))
        (stop dispatcher)))))
