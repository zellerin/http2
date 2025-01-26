(cl:in-package :http2/tests)

(defpackage #:http2/tests/high-level
  (:use #:cl #:fiasco #:http2/server #:http2/client))

(in-package #:http2/tests/high-level)

(fiasco:defsuite
    (http2/tests/high-level :bind-to-package #:http2/tests/high-level
                 :in http2/tests::http2/tests))

(deftest client-server-match ()
  "Run server, fetch a response from it."
  (multiple-value-bind (response code)
      (multiple-value-bind (thread url)
          (start 0)
        (unwind-protect
             (retrieve-url url)
          (bordeaux-threads:destroy-thread thread)))
    (is (= code 404))
    (is (search "Not found" response))))

(define-exact-handler "/hello-world"
  (handler (foo :utf-8 nil)
    (with-open-stream (foo foo)
      (send-headers
       '((:status "200")
         ("content-type" "text/html; charset=utf-8")))
      (format foo "Hello World, this is random: ~a" (random 10)))))

(deftest tutorial-server-content ()
  "Run server, fetch a response from it."
  (multiple-value-bind (response code)
      (multiple-value-bind (thread url)
          (start 0)
        (unwind-protect
             (retrieve-url (puri:merge-uris "/hello-world" url))
          (bordeaux-threads:destroy-thread thread)))
    (is (= code 200))
    (is (search "Hello World, this is random" response))))

(define-exact-handler "/body"
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
  (multiple-value-bind (thread url)
      (start 0)
    (unwind-protect
         (progn
           (multiple-value-bind (response code)
               (retrieve-url (puri:merge-uris "/body" url)
                             :content "ABC")
             (is (= code 200))
             (is (equal response "POST request; ABC ((\"content-type\" . \"text/plain; charset=utf-8\")); NIL")))
           (multiple-value-bind (response code)
               (retrieve-url (puri:merge-uris "/body" url)
                             :content #(1 2 3))
             (is (= code 200))
             (is (equal response "POST request;  ((\"content-type\" . \"application/octet-stream\")); #(1 2 3)"))))
      (bordeaux-threads:destroy-thread thread))))
