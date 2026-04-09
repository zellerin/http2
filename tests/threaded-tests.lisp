(in-package :http2)

(fiasco:defsuite
    (http2 :bind-to-package #:http2
                 :in http2/tests::http2/tests))

(defsection @tests
    (:title "Tests overview")
  (@test-server section))

(defsection @test-server
    (:title "Tests server")
  "For tests, there is a test server running in a separate thread *TEST-SERVER-THREAD* on url *SERVER-URL*."
  (with-test-server macro)
  (*test-server-thread* variable)
  (*server-url* variable)
  (kill-background-server function)
  (to-srv-port function))

(defvar *server-domain* "localhost")
(defvar *server-url* nil
  "Port with running test server. This is set dynamically when the server starts.")
(defvar *test-server-thread* nil)

(defvar *test-webs-external*
  '(("https://example.com" "<title>Example Domain</title>" 200)
    ("https://lupa.cz" "Moved Permanently" 301)
    ("https://www.seznam.cz" "" 200))
  "List of tripples for testing the client: URL, text on page and status code.")

(defvar *test-webs-internal*
  '(("/foo" "Not found" 404)
    ("/demo/ok" "OK" 200)))

(defun to-srv-port (fragment)
  (puri:merge-uris fragment *server-url*))

(defun test-webs (webs)
  "Test WEBS (a list of relative url, text snippet to see in the page and response status)"
  (loop for (page search-term code) in webs
        do
           (multiple-value-bind (body status)
               (http2/client::retrieve-url (puri:merge-uris page *server-url*))
             (fiasco:is (search search-term body)
                 "Page ~a does not contain ~a (is ~s)" page search-term body)
             (fiasco:is (equal code status)
                 "Page ~a does not have status ~a, but ~a" page code status))))

(defun call-with-test-server (fn)
  (setq *server-url* nil)
  (multiple-value-bind (server *server-url*)
      (http2/server:start 0)
    (unwind-protect
         (restart-bind
             ((break-in-server (lambda ()
                                 (bt:interrupt-thread *test-server-thread* #'break))))
           (funcall fn))
      (http2/server:stop server)
      (setf *server-url* nil *test-server-thread* nil))))

(defmacro with-test-server  (&body body)
  `(call-with-test-server (lambda () ,@body)))

(fiasco:deftest test-self-compatible ()
  (with-test-server
    (test-webs *test-webs-internal*)))

(defun test-curl (url content)
  (fiasco:is (search content (uiop:run-program `("/usr/bin/curl" ,(puri:render-uri (puri:merge-uris url *server-url*) nil) "-k") :output :string))))

(fiasco:deftest test-curl-access ()
  (with-test-server
    (test-curl "/ok" "Redirect was OK")))

(fiasco:deftest test-post ()
  (with-test-server
    (fiasco:is (equal "AB" (http2/client:retrieve-url
                            (to-srv-port "/demo/body")
                            :method "POST"
                            :content #(65 66)
                            :content-type "binary/something")))
    (fiasco:is (equal "AB" (http2/client:retrieve-url
                            (to-srv-port "/demo/body")
                            :method "POST"
                            :content "AB")))
    (fiasco:is (equal "AB" (http2/client:retrieve-url
                            (to-srv-port "/demo/body")
                            :method "POST"
                            :content "AB"
                            :gzip-content t)))
    (fiasco:is (equal "AB"
                      (http2/client:retrieve-url
                       (to-srv-port "/demo/body")
                       :method "POST"
                       :additional-headers '(("content-type" . "text/plain; charset=UTF-8"))
                       :content-fn
                       (lambda (out)
                         (write-sequence "AB" out)))))))
