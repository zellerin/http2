#!/usr/bin/env sbcl --script

(load "~/.sbclrc")
(asdf::load-asd (truename "./http2.asd"))
(ql:quickload "http2/server" :silent t)
(ql:quickload "hunchentoot" :silent t)
(ql:quickload 'cl-ppcre :silent t)
(ql:quickload 'woo :silent t)


(in-package :http2/server)

(define-exact-handler "/"
    (send-text-handler "/Hello World"))

(start 1237)
(start 1240 :dispatcher 'http2/server::detached-poll-dispatcher)
(start 1241 :dispatcher 'http2/server::detached-tls-threaded-dispatcher)
(start 1238 :dispatcher 'http2/server::detached-threaded-dispatcher)
(start 1255 :dispatcher 'http2/server::detached-single-client-dispatcher)

(setf hunchentoot:*log-lisp-backtraces-p* nil)

(bt:make-thread
 (lambda ()
   (woo:run
    (lambda (env)
      (declare (ignore env))
      '(200 (:content-type "text/plain") ("Hello, World")))
    :port 1239)))

(maybe-create-certificate  "/tmp/server.key" "/tmp/server.crt")
(hunchentoot:start (make-instance 'hunchentoot:easy-ssl-acceptor
                                  :port 1235
                                  :ssl-privatekey-file "/tmp/server.key"
                                  :ssl-certificate-file "/tmp/server.crt"
                                  :access-log-destination nil))

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 1236
                                                             :access-log-destination nil))
(flet ((scan-port (base name &rest args )
         (let ((res (uiop:run-program `("h2load" "-D1"
                                                 ,@args)
                                      :output :string)))
           (multiple-value-bind (match val)
               (cl-ppcre:scan-to-strings "req/s[ :]*([0-9\\.]+)[ ]*([0-9\\.]+)[ ]*([0-9\\.]+)" res)
             (let ((val-as-num (read-from-string (aref val 2))))
               (format t "~50a ~10@a~@[~5@a%~]~%" name val-as-num
                       (when base (floor (/ val-as-num base 0.01))))
               val-as-num)))))

  (let ((base (scan-port nil "Hunchetoot over https" "https://localhost:1235")))
    (scan-port base "HTTP/2 over TLS" "https://localhost:1237")
    (scan-port base "HTTP/2 over plain socket" "http://localhost:1238")
    (scan-port base "Hunchentoot on plain socket" "http://localhost:1236" "-p" "http/1.1")
    (scan-port base "Woo on plain socket" "http://localhost:1239" "-p" "http/1.1")
    (scan-port base "HTTP/2 over plain socket, -m20" "http://localhost:1238"
               "-m" "20")
    (scan-port base "HTTP/2 over tls socket, -m20" "https://localhost:1241"
                   "-m" "20")
    (scan-port base "HTTP/2 single client over plain socket, -m20" "http://localhost:1255"
               "-m" "20")
    (scan-port base "HTTP/2 poll single-thread server, -m20" "https://localhost:1240"
               "-m" "20")
    (scan-port base "Hunchentoot on plain socket, -m20" "http://localhost:1236"
               "-p" "http/1.1" "-m" "20" )

    (scan-port base "Woo on plain socket, -m20" "http://localhost:1239" "-p" "http/1.1"
               "-m" "20" "-D1") ;this hangs when count-based for obvious reasons
#+nil    (scan-port base "HTTP/2, plain socket, post " "http://localhost:1238"
               "-d" "./client/client.lisp")
    (scan-port base "Hunchentoot on plain socket, post" "http://localhost:1236" "-p" "http/1.1"
               "-d" "./client/client.lisp")))
