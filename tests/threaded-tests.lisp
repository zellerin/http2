(in-package :http2)

(defsection @tests
    (:title "Tests overview")
  (@test-server section))

(defsection @test-server
    (:title "Tests server")
  "For tests, there is a test server running in a separate thread *TEST-SERVER-THREAD* on urkl *SERVER-URL*."
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
    ("/ok" "OK" 200)))

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

(defun call-with-test-server (dispatcher fn)
  (setq *server-url* nil)
  (let ((*test-server-thread*
          (bt:make-thread
           (lambda ()
             (handler-bind ((warning 'muffle-warning)
                            (error (lambda (e)
                                     (declare (ignore e))
                                     (when (find-restart 'close-connection)
                                       (invoke-restart 'close-connection)))))
               (http2/server-example:maybe-create-certificate  "/tmp/server.key" "/tmp/server.crt")
               (http2:create-server 0 dispatcher
                                    :announce-url-callback (lambda (a)
                                                             (setf *server-url* a)
                                                             t))))
           :name "HTTP2 server")))
    #+sbcl(sb-ext:wait-for *server-url* :timeout 5)
    #-sbcl (loop for i from 0 to 50
                 until *server-url*
                 dooo (sleep 0.1))
    (unwind-protect
         (restart-bind
             ((break-in-server (lambda ()
                                 (bt:interrupt-thread *test-server-thread* #'break))))
           (funcall fn))
      (bt:interrupt-thread *test-server-thread* #'invoke-restart 'kill-server)
      (setf *server-url* nil *test-server-thread* nil))))

(defmacro with-test-server (server-parameters &body body)
  `(call-with-test-server (make-instance ,@server-parameters)
                          (lambda () ,@body)))

(fiasco:deftest test-self-compatible ()
  (with-test-server ('tls-single-client-dispatcher)
    (test-webs *test-webs-internal*)))

(defun test-curl (url content)
  (fiasco:is (search content (uiop:run-program `("/usr/bin/curl" ,(puri:render-uri (puri:merge-uris url *server-url*) nil) "-k") :output :string))))

(fiasco:deftest test-curl-access ()
  (with-test-server ('tls-single-client-dispatcher)
    (test-curl "/ok" "Redirect was OK")))

(fiasco:deftest test-post ()
  (with-test-server ('tls-single-client-dispatcher)
    (fiasco:is (equal "AB" (http2/client:retrieve-url
                            (to-srv-port "/body")
                            :method "POST"
                            :content #(65 66)
                            :content-type "binary/something")))
    (fiasco:is (equal "AB" (http2/client:retrieve-url
                            (to-srv-port "/body")
                            :method "POST"
                            :content "AB")))
    (fiasco:is (equal "AB" (http2/client:retrieve-url
                            (to-srv-port "/body")
                            :method "POST"
                            :content "AB"
                            :gzip-content t)))
    (fiasco:is (equal "AB"
                      (http2/client:retrieve-url
                       (to-srv-port "/body")
                       :method "POST"
                       :additional-headers '(("content-type" . "text/plain; charset=UTF-8"))
                       :content-fn
                       (lambda (out)
                         (write-sequence "AB" out)))))))

(fiasco:deftest test-post-2 ()
)

(fiasco:deftest test-ping ()
  (with-test-server ('tls-single-client-dispatcher)
    (flet ((test-ping-returns (&rest pars)
             (fiasco:is (search "Ping time:"
                                (with-output-to-string (*standard-output*)
                                  (apply #'http2/client:retrieve-url pars))))))
      (test-ping-returns (to-srv-port "/ok") :ping 3)
      (test-ping-returns (to-srv-port "/ok") :ping t))))
