;;;; Copyright 2022 by Tomáš Zellerin

;;;; Create a specific server. Use dispatch handlers to define behaviour of the
;;;; server, and actually bind it to a TLS socket.

(defpackage :http2/server
  (:use :cl :http2))

(in-package :http2/server)

(define-prefix-handler "/re" (redirect-handler "/ok"))

(define-exact-handler "/ok" (send-text-handler "Redirect was OK"
                                               :content-type "text/plain"
                                               :additional-headers '(("refresh" "3; url=/"))))

(defvar *intro-page*
  "<h1>Hello World</h1>
<p>This server is for testing http2 protocol</p>
<p><a href='/redir'>Redirect test</a>
<form action='/body' method='post'><input type='submit' name='xxx' value='POST query test'></form")

(require 'cl-who)
(define-exact-handler "/"
    (handler (out :external-format :utf-8)
      (send-headers `((:status "200") ("content-type" "text/html; charset=utf-8")))
      (cl-who:with-html-output (out)
        (:h1 "Hello World")
        (:p "This server is for testing http2 protocol")
        (:p (:a :href "/redir" "Redirect test") " "
            (:a :href "/long" "Long page test"))
        (:form :action "/body" :method "post"
               (:input :type :submit :name "xxx" :value "POST query test"))
        (:p "UTF8 test: Příliš žluťoučký kůň... 😎"))))

(define-exact-handler "/long"
    (handler (out)
      (send-headers `((:status "200") ("content-type" "text/html")
                      ("refresh" "30; url=/")))
      (cl-who:with-html-output (out)
        (:h1 "Test long body")
        (dotimes (i 100000)
          (cl-who:htm (:p "A paragraph #" (princ (format nil "~d" i) out) "."))))))

(define-exact-handler "/body"
    (handler (out)
      (send-headers `((:status "200") ("content-type" "text/plain")
                      ("refresh" "3; url=/")))
      (princ (get-body stream) out)))

(defun main ()
  (create-https-server 1230 "/tmp/server.key" "/tmp/server.crt"
                       :verbose nil))
