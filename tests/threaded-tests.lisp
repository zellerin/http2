(in-package :http2)

(defvar *server-running* nil)
(defvar *test-server-thread*
  (bt:make-thread
   (lambda ()
     (handler-bind ((warning 'muffle-warning))
       (http2:create-https-server 1230
                                  "/tmp/server.key" "/tmp/server.crt"
                                  :announce-open-fn (lambda ()
                                                      (setf *server-running* t)))))
   :name "HTTP2 server"))

(defun kill-background-server ()
  "Kill running background server"
  (bt:interrupt-thread *test-server-thread* #'invoke-restart 'kill-server))

#+sbcl(sb-ext:wait-for *server-running* :timeout 5)
#-sbcl (loop for i from 0 to 50
             until *server-running*
             do (sleep 0.1))

(fiasco:deftest test-self-compatible ()
  (test-webs '(("https://localhost:1230/foo" "Not found" "404")
               ("https://localhost:1230/ok" "OK" "200"))))

(defun test-curl (url content)
  (fiasco:is (search content (uiop:run-program `("/usr/bin/curl" ,url "-k") :output :string))))

(fiasco:deftest test-curl-access ()
  (test-curl "https://localhost:1230/ok" "Redirect was OK"))

(fiasco:deftest test-post ()
  (fiasco:is (equal "AB" (http2/client:retrieve-url "https://localhost:1230/body" :method "POST" :content #(65 66)))))

(fiasco:deftest test-post-2 ()
  (fiasco:is (equal "AB"
                    (http2/client:retrieve-url "https://localhost:1230/body"
                                               :method "POST"
                                               :content-fn
                                               (lambda (out) (write-sequence "AB" out))))))

(fiasco:deftest test-ping ()
  (flet ((test-ping-returns (&rest pars)
           (fiasco:is (search "Ping time:"
                              (with-output-to-string (*standard-output*)
                                (apply #'http2/client:retrieve-url pars))))))
    (test-ping-returns "https://localhost:1230/ok" :ping 3)
    (test-ping-returns "https://localhost:1230/ok" :ping t)))
