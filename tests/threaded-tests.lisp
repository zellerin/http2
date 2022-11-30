(in-package :http2)

(defvar *server-port* nil)
(defvar *test-server-thread*
  (bt:make-thread
   (lambda ()
     (handler-bind ((warning 'muffle-warning))
       (http2:create-https-server 0
                                  "/tmp/server.key" "/tmp/server.crt"
                                  :announce-open-fn (lambda (a)
                                                      (setf *server-port*
                                                            (usocket:get-local-port a))
                                                      t))))
   :name "HTTP2 server"))

(defun kill-background-server ()
  "Kill running background server"
  (bt:interrupt-thread *test-server-thread* #'invoke-restart 'kill-server))

#+sbcl(sb-ext:wait-for *server-port* :timeout 5)
#-sbcl (loop for i from 0 to 50
             until *server-port*
             do (sleep 0.1))

(defun to-srv-port (uri)
  "URI moved to the *SERVER-PORT*"
  (puri:copy-uri (puri:parse-uri uri) :port *server-port*))

(fiasco:deftest test-self-compatible ()
  (test-webs `((,(to-srv-port "https://localhost/foo")
                "Not found" 404)
               (,(to-srv-port "https://localhost/ok")
                "OK" 200))))

(defun test-curl (url content)
  (fiasco:is (search content (uiop:run-program `("/usr/bin/curl" ,(puri:render-uri url nil) "-k") :output :string))))

(fiasco:deftest test-curl-access ()
  (test-curl (to-srv-port "https://localhost/ok") "Redirect was OK"))

(fiasco:deftest test-post ()
  (fiasco:is (equal "AB" (http2/client:retrieve-url
                          (to-srv-port "https://localhost/body")
                          :method "POST"
                          :content #(65 66)
                          :content-type "binary/something")))
    (fiasco:is (equal "AB" (http2/client:retrieve-url
                          (to-srv-port "https://localhost/body")
                          :method "POST"
                          :content "AB")))
  (fiasco:is (equal "AB" (http2/client:retrieve-url
                          (to-srv-port "https://localhost/body")
                          :method "POST"
                          :content "AB"
                          :gzip-content t))))

(fiasco:deftest test-post-2 ()
  (fiasco:is (equal "AB"
                    (http2/client:retrieve-url
                     (to-srv-port "https://localhost/body")
                     :method "POST"
                     :additional-headers '(("content-type" . "text/plain; charset=UTF-8"))
                     :content-fn
                     (lambda (out)
                       (write-sequence "AB" out))))))

(fiasco:deftest test-ping ()
  (flet ((test-ping-returns (&rest pars)
           (fiasco:is (search "Ping time:"
                              (with-output-to-string (*standard-output*)
                                (apply #'http2/client:retrieve-url pars))))))
    (test-ping-returns (to-srv-port "https://localhost/ok") :ping 3)
    (test-ping-returns (to-srv-port "https://localhost/ok") :ping t)))
