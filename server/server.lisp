;;;; Copyright 2022 by Tomáš Zellerin

;;;; Create a specific server. Use dispatch handlers to define behaviour of the
;;;; server, and actually bind it to a TLS socket.

(defpackage :http2/server
  (:use :cl :http2))

(in-package :http2/server)

(define-prefix-handler "/re" (redirect-handler "/ok"))

(define-exact-handler "/ok" (send-text-handler "Redirect was OK"
                                               :content-type "text/plain; charset=UTF-8"
                                               :additional-headers '(("refresh" "3; url=/"))))

(define-exact-handler "/"
    (handler (out :external-format :utf-8)
      (send-headers `((:status "200") ("content-type" "text/html; charset=utf-8")))
      (cl-who:with-html-output (out)
        (:h1 "Hello World")
        (:p "This server is for testing http2 protocol implementation")
        (:ul
         (:li (:a :href "/redir" "Redirect test")) " "
         (:li (:a :href "/long" "Long page test")) " "
         (:li (:a :href "/longerslow" "Slowly printing page") " (test with curl -N)"))
        (:form :action "/body" :method "post"
               (:input :type :submit :name "xxx" :value "POST query test"))
        (:p "UTF8 test: Příliš žluťoučký kůň... 😎"))))

(define-exact-handler "/long"
    (handler (out)
      (send-headers `((:status "200") ("content-type" "text/html; charset=utf-8")
                      ("refresh" "30; url=/")))
      (cl-who:with-html-output (out)
        (:h1 "Test long body")
        (dotimes (i 100000)
          (cl-who:htm (:p "A paragraph #" (princ (format nil "~d" i) out) "."))))))

(define-exact-handler "/longerslow"
    (handler (out)
      (send-headers `((:status "200") ("content-type" "text/html; charset=utf-8")
                      ("refresh" "30; url=/")))
      (cl-who:with-html-output (out)
        (:h1 "Test longer body")
        (dotimes (i 10)
          (cl-who:htm (:p "A paragraph #" (princ (format nil "~d" i) out) "."))
          (terpri out)
          (force-output out)
          (sleep 1)))))

(define-exact-handler "/body"
    (handler (out)
      (send-headers `((:status "200") ("content-type" "text/plain; charset=utf-8")
                      ("refresh" "3; url=/")))
      (princ (get-body stream) out)))

(defclass no-handler-connection (vanilla-server-connection)
  ()
  (:default-initargs :exact-handlers nil :prefix-handlers nil)
  (:documentation
   "A variant of the vanilla-server-connection without any default handlers."))

(defun create-one-shot-server (handler port)
  "Open server on PORT that handles just one request and returns value from HANDLER.

The original use case is server for oauth2 authentication redirect, there might
be other ones."
  (let ((*dispatch-fn*
          (lambda (p-s-s stream &key &allow-other-keys)
            (declare (ignore p-s-s))
            (let ((connection (make-instance 'no-handler-connection
                                             :network-stream stream)))
              (define-exact-handler "/"
                  (lambda (conn http-stream)
                    (let ((value (funcall handler conn http-stream)))
                      (write-goaway-frame connection 0 +no-error+ #())
                      (invoke-restart 'http2::kill-server value)))
                connection)
              (process-server-stream stream :connection connection)))))
    (create-https-server port "/tmp/server.key" "/tmp/server.crt")))
