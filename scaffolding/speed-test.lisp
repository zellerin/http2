

(load "~/quicklisp/setup")
(asdf::load-asd (truename "./http2.asd"))
(ql:quickload "http2/server/example")
(ql:quickload "hunchentoot")

(in-package :http2/server-example)

(bt:make-thread (lambda () (http2/server-example::run-demo-server :port 1237)) :name "Our server thread")

(hunchentoot:start (make-instance 'hunchentoot:easy-ssl-acceptor :port 1235 :ssl-privatekey-file "/tmp/server.key" :ssl-certificate-file "/tmp/server.crt"
                                                                 :access-log-destination nil))

(flet ((scan-port (port)
         (uiop:run-program `("h2load" "-n" "10000" "-c" "1" "-m" "1" ,(format nil "https://localhost:~d" port)) :output *standard-output*)))
  (scan-port 1235)
  (scan-port 1237))
