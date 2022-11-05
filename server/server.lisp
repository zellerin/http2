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

(define-exact-handler "/es-test"
    (handler (out)
      (send-headers `((:status "200") ("content-type" "text/html; charset=utf-8")
                      ("refresh" "30; url=/")))
      (cl-who:with-html-output (out out :prologue "<!DOCTYPE html>")
        (:html
         (:body
          "Waiting for event"
          (:p :id "events")
          (:script :type "text/javascript"
                   (cl-who:str
                    (parenscript:ps
                      (let ((source (ps:new (-event-source "/event-stream"))))
                        (setf (ps:@ source onmessage)
                              (lambda (event)
                                (ps:setf (ps:@ ((ps:@ document get-element-by-id) "events") inner-h-t-m-l)
                                         (ps:@ event data))
                                (values)) )
                        (values))))))))))

(define-exact-handler "/event-stream"
    (handler (out :external-format '(:utf-8 :eol-style :crlf))
      (send-headers `((:status "200") ("content-type" "text/event-stream")))
      (handler-case
          (loop for i from 1
                do
                   (sleep 1)
                   (format out "id: ~d~%" i)
                   (multiple-value-bind (sec min hr day #+nil (month year))
                       (decode-universal-time (get-universal-time))
                     (format out "data: ~2,'0dT~2,'0d:~2,'0d:~2,'0d~2%" day hr min sec))
                   (force-output out)))))

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

(define-exact-handler "/test"
    (handler (out)
      (send-headers `((:status "200") ("content-type" "text/html; charset=utf-8")))
      (cl-who:with-html-output (out)
        (:h1 "Automated test page")
        (:table
         (:tr (:th "Test") (:th "Status"))
         (dolist (test '("404" "Body passing" "Long" "Slow print"
                         "Event stream"))
           (cl-who:htm (:tr (:td (cl-who:str test)) (:td :id test "TODO")))))
        (:script
         (cl-who:str (ps:ps
                       (defun set-result (test result)
                         (setf (ps:@ ((ps:@ document get-element-by-id) test)
                                     inner-h-t-m-l)
                               result))

                       (defun test-request (name check-fn method page &optional body)
                         (let ((req (ps:new *X-M-L-Http-Request)))
                           ((ps:@ req add-event-listener)
                            "load"
                            (lambda ()
                              (set-result name
                                          (let ((res (funcall check-fn this)))
                                            (if res  (+ "BAD: " res)  "PASS")))))
                           ((ps:@ req add-event-listener)
                            "error"
                            (lambda () (set-result name "ERROR")))
                           ((ps:@ req open) method page)
                           ((ps:@ req send) body)))

                       (test-request "404"
                                     (lambda (reply)
                                       (unless (= 404 (ps:@ reply status))
                                         (ps:@ reply status)))
                        "GET" "/no-such-page")

                       (test-request "Body passing"
                        (lambda (reply)
                          (unless
                              (and
                               (= 200 (ps:@ reply status))
                               (= "SAMPLE BODY" (ps:@ reply response-text)))
                            (+ (ps:@ reply status)
                               " "  (ps:@ reply response-text))))
                        "POST" "/body"  "SAMPLE BODY")

                       (test-request "Long"
                        (lambda (reply)
                          (unless (and
                               (= 200 (ps:@ reply status))
                               (= 2588913 (length (ps:@ reply response-text))))
                            (+ "BAD: code " (ps:@ reply status)
                               " length "  (length (ps:@ reply response-text)))))
                        "GET" "/long")

                       (let ((source (ps:new (-event-source "/event-stream"))))
                         (setf (ps:@ source onmessage)
                               (lambda (event)
                                 (ps:setf (ps:@ ((ps:@ document get-element-by-id) "Event stream") inner-h-t-m-l)
                                          (ps:@ event data))
                                 (when (= "5"
                                          (ps:@ event last-event-id))
                                   ((ps:@ source close))
                                   (set-result "Event stream" "PASS"))
                                 (values)))
                         (values))
                       ))))))

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
