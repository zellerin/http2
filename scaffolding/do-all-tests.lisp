;;;; Copyright 2022 by Tomáš Zellerin

(load "~/quicklisp/setup")
#+sbcl (require :sb-cover)
(asdf::load-asd (truename "./http2.asd"))
(ql:quickload "fiasco")
(ql:quickload "cl+ssl")
(ql:quickload "puri")
#+sbcl (declaim (optimize sb-cover:store-coverage-data))
(asdf:initialize-output-translations
         `(:output-translations
           (,(merge-pathnames "**/*.*" (asdf:system-source-directory "http2")) #p"/tmp/fasl/pre-commit-cache/**/*.*")
           :inherit-configuration :enable-user-cache))
(ql:quickload "http2/all")


(in-package http2)

(fiasco:deftest test-client-server ()
  (multiple-value-bind (client-stream server-stream) (make-full-pipe)
    ;; order matters: client needs to write client message so that server can read it.
    (let ((client (make-instance 'http2/client::vanilla-client-connection :network-stream client-stream))
          (server (make-instance 'vanilla-server-connection :network-stream server-stream)))
      (send-headers client
                    (request-headers "GET"  "/" "localhost")
                           :end-stream t)
      (process-messages (list client server))
      (let ((client-stream (car (get-streams client))))
        (fiasco:is (eq (get-state client-stream) 'closed))
        (fiasco:is (search "Hello World" (get-body client-stream)))))))

(defvar *server-running* nil)
(defvar *test-server-thread*
  (bt:make-thread (lambda ()
                    (handler-bind ((warning 'muffle-warning))
                      (http2:create-https-server 1230 "/tmp/server.key" "/tmp/server.crt"
                                                 :announce-open-fn (lambda ()
                                                                     (setf *server-running* t)))))))

(fiasco::run-package-tests :package '#:http2 )

#+sbcl (handler-bind ((warning #'muffle-warning))
  (sb-cover:report (merge-pathnames
                    "cover-report/" (asdf:system-source-directory "http2"))))
