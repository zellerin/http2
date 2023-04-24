;;;; Copyright 2022, 2023 by Tomáš Zellerin

;;;; Create a specific server. Use dispatch handlers to define behaviour of the
;;;; server, and actually bind it to a TLS socket.

(defpackage :http2/server-example
  (:use :cl :http2 :cl-who :ps))

(in-package :http2/server-example)

(define-prefix-handler "/re" (redirect-handler "/ok"))

(define-exact-handler "/ok"
    (send-text-handler "Redirect was OK"
                       :content-type "text/plain; charset=UTF-8"
                       :additional-headers '(("refresh" "3; url=/"))
                       :gzip nil))

(define-exact-handler "/"
    (handler (out :utf-8 nil)
      (send-headers `((:status "200") ("content-type" "text/html; charset=utf-8")))
      (with-html-output (out)
        (:h1 "Hello World")
        (:p "This server is for testing http2 protocol implementation")
        (:ul
         (:li (:a :href "/redir" "Redirect test")) " "
         (:li (:a :href "/long" "Long page test")) " "
         (:li (:a :href "/test" "A test page") ""))
        (:form :action "/body" :method "post"
               (:input :type :submit :name "xxx" :value "POST query test"))
        (:p "UTF8 test: Příliš žluťoučký kůň... 😎"))))

(define-exact-handler "/long"
    (handler (out :utf-8 nil)
      (send-headers `((:status "200") ("content-type" "text/html; charset=utf-8")
                      ("refresh" "30; url=/")))
      (with-html-output (out)
        (:h1 "Test long body")
        (dotimes (i 100000)
          (htm (:p "A paragraph #" (princ (format nil "~d" i) out) "."))))))

(define-exact-handler "/es-test"
    (handler (out :utf-8 nil)
      (send-headers `((:status "200") ("content-type" "text/html; charset=utf-8")
                      ("refresh" "30; url=/")))
      (with-html-output (out out :prologue "<!DOCTYPE html>")
        (:html
         (:body
          "Waiting for event"
          (:p :id "events")
          (:script :type "text/javascript"
                   (str
                    (parenscript:ps
                      (let ((source (new (-event-source "/event-stream"))))
                        (setf (@ source onmessage)
                              (lambda (event)
                                (setf (@ ((@ document get-element-by-id) "events")
                                         text-content)
                                      (@ event data))
                                (values)) )
                        (values))))))))))

(define-exact-handler "/event-stream"
    (scheduling-handler (out '(:utf-8 :eol-style :crlf) nil)
      (send-headers `((:status "200") ("content-type" "text/event-stream")))
      (let ((i 0))
        (labels ((send-event-and-plan-next ()
                   (handler-case
                       (bt:with-lock-held ((get-lock connection))
                         (format out "id: ~d~%" (incf i))
                         (multiple-value-bind (sec min hr day)
                             (decode-universal-time (get-universal-time))
                           (format out "data: ~2,'0dT~2,'0d:~2,'0d:~2,'0d~2%" day hr
                                   min sec))
                         (force-output out)
                         (schedule-task (get-scheduler connection) 1000000
                                        #'send-event-and-plan-next))
                     ; TODO: handle stream closed
                     )))
          ;; this needs to be scheduled so that existing lock is not re-acquired
          (schedule-task (get-scheduler connection) 0
                         #'send-event-and-plan-next)))))

(define-exact-handler "/body"
    (handler (out :utf-8 nil)
      (send-headers `((:status "200") ("content-type" "text/plain; charset=utf-8")
                      ("refresh" "3; url=/")))
      (princ (get-body stream) out)))

(defmethod http2::add-header (connection (stream http2::server-stream) name value)
  (handler-bind ((warning #'muffle-warning))
    (call-next-method)))

(defparameter *tests*
  `(("404" "Test that the response of not-found is 404"
           (test-request "404"
                         (lambda (reply)
                           (unless (= 404 (@ reply status))
                             (@ reply status)))
                         "GET" "/no-such-page"))
    ("Body passing" "Test that body is available to the server."
                    (test-request "Body passing"
                                  (lambda (reply)
                                    (unless
                                        (and
                                         (= 200 (@ reply status))
                                         (= "SAMPLE BODY" (@ reply response-text)))
                                      (+ (@ reply status)
                                         " "  (@ reply response-text))))
                                  "POST" "/body"  "SAMPLE BODY"))
    ("Long" "Test that long responses are handled well (streams over http streams)."
            (test-request "Long"
                          (lambda (reply)
                            (unless (and
                                     (= 200 (@ reply status))
                                     (= 2588913 (length (@ reply response-text))))
                              (+ "BAD: code " (@ reply status)
                                 " length "  (length (@ reply response-text)))))
                          "GET" "/long"))
    ("Event stream" "Test event streams - scheduler and locking"
                    (let ((source (new (-event-source "/event-stream"))))
                      (setf (@ source onmessage)
                            (lambda (event)
                              (setf (@ ((@ document get-element-by-id) "Event stream") inner-h-t-m-l)
                                    (@ event data))
                              (when (= "5"
                                       (@ event last-event-id))
                                ((@ source close))
                                (set-result "Event stream" "PASS" "ok"))
                              (values)))
                      (values)))
    ("1000 events" "Test sending 1000 parallel requests"
                   (let ((res 0))
                     (dotimes (i 1000)
                       (let ((req (new *X-M-L-Http-Request)))
                         ((@ req add-event-listener)
                          "load"
                          (lambda ()
                            (incf res)
                            (cond
                              ((= res 1000)
                                (set-result "1000 events"
                                            "PASS" "ok"))
                              ((> res 1000)
                                (set-result "1000 events"
                                            "FAIL (too much)" "nok"))
                              ((< res 1000)
                                (set-result "1000 events"
                                            res "todo")))))
                         ((@ req add-event-listener)
                          "error"
                          (lambda () (set-result "1000 events" "ERROR" "nok")))
                         ((@ req open) "GET" "/")
                         ((@ req send)))))))
  "Tests to be executed in the browser on /test page. Each item is test name, test description and test code in Parenscript.")

(defparameter *test-stylesheet*
  "tr.ok   {background-color: LimeGreen}
   tr.nok  {background-color: LightCoral}
   tr.todo {background-color: Yellow}
   tr      {background-color: LightBlue}

   td     { padding: 0.1em 1em}
   th.res { padding: 0.1em 3em 0.1em 3em}
   td.res { text-align: center}

   table,th,td {border: 1px solid black}
   table {border-collapse: collapse}"

  "Rudimental CSS to prettify and colorize the results table in /test.

Complication solved by padding is that the intermediate yellow text can be
longer than PASS text and table width changes on fly.")

(defun print-test-table (out)
  "Print result table for the tests with all fields in TODO state."
  (with-html-output (out)
    (:table
     (:tr (:th "Test") (:th :class "res" "Status") (:th "Comment"))
     (dolist (test *tests*)
       (htm (:tr :class "todo" (:td (str (car test)))
                 (:td :id (car test) :class "res" "TODO")
                 (:td (str (second test)))))))))

(defparameter *js-test-helpers*
  (ps
    (defun set-result (test result res-class)
      (setf (@ ((@ document get-element-by-id) test)
               text-content)
            result
            (@ ((@ document get-element-by-id) test)
               parent-element class-name)
            res-class))

    (defun test-request (name check-fn method page &optional body)
      (let ((req (new *X-M-L-Http-Request)))
        ((@ req add-event-listener)
         "load"
         (lambda ()
           (let ((res (funcall check-fn this)))
             (set-result name
                         (if res  (+ "BAD: " res)  "PASS")
                         (if res "nok" "ok")))))
        ((@ req add-event-listener)
         "error"
         (lambda () (set-result name "ERROR")))
        ((@ req open) method page)
        ((@ req send) body))))

  "Helper functions for ")

(define-exact-handler "/test"
    (handler (out :utf-8 t)
      (send-headers `((:status "200")
                      ("content-type" "text/html; charset=utf-8")
                      ("content-encoding" "gzip")))
      (with-html-output (out out :prologue "<!DOCTYPE html>")
        (:html
         (:header (:style (esc *test-stylesheet*)))
         (:body :onload "doTests()"
                (:h1 "HTTP/2 demo server test page")
                (:p "Run several tests of the server over Javascript. After few seconds, all yellow rows in the table below should turn green.")
                (print-test-table out)
                (:script
                 (str *js-test-helpers*)
                 (str
                  (ps*
                   `(defun do-tests ()
                      ,@(mapcar 'third *tests*))))))))))

(defvar *default-certificate-pair*
  '("/tmp/server.key" "/tmp/server.crt")
  "Path to files with the default private key and certificate to use for server.")

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
                      (invoke-restart 'kill-server value)))
                connection)
              (process-server-stream stream :connection connection)))))
    (apply #'create-https-server port *default-certificate-pair*)))

(defun maybe-create-certificate (key certificate)
  "Generate key and a self-signed certificate to it for localhost using openssl
cli."
  (unless (and (probe-file key)
               (probe-file certificate))
    (format t "~%Generating temporary certificates")
    (uiop:run-program "openssl req -new -nodes -x509 -days 365 -subj /CN=localhost -keyout /tmp/server.key -outform PEM -out /tmp/server.crt")
    (terpri)))

(defun run-demo-server (&key (key "/tmp/server.key")
                          (certificate "/tmp/server.crt")
                          (port 1230))
  "Start a http2 server on localhost on PORT that servers some test content.

Create certficates if they do not exist.

Do something (see code) with conditions."

  (maybe-create-certificate key certificate)
  (handler-bind ((warning 'muffle-warning)
                 (error (lambda (e)
                          (describe e)
                          (invoke-restart 'abort))))
    (create-https-server port key certificate)))
