;;;; Copyright 2022, 2023 by Tomáš Zellerin

;;;; Create a specific server. Use dispatch handlers to define behaviour of the
;;;; server, and actually bind it to a TLS socket.

(defpackage :http2/server-example
  (:use :cl :http2 :cl-who :ps)
  (:export #:maybe-create-certificate
           #:run-demo-server #:create-one-shot-server))

(in-package :http2/server-example)

(define-prefix-handler "/re" (redirect-handler "/ok"))

(define-exact-handler "/ok"
    (send-text-handler "Redirect was OK"
                       :content-type "text/plain; charset=UTF-8"
                       :additional-headers '(("refresh" "3; url=/"))
                       :gzip nil))

(define-exact-handler "/slow"
    (handler (foo :utf-8 nil)
      (send-headers
       '((:status "200")
         ("content-type" "text/html; charset=utf-8")))
      (princ "Hello World" foo)))


(define-exact-handler "/"
    (constant-handler
     (out :utf-8 nil
          `((:status "200")
            ("content-type" "text/html; charset=utf-8")))
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

(define-exact-handler "/long2"
    (constant-handler (out :utf-8 t
                               `((:status "200") ("content-type" "text/html; charset=utf-8")
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

(defmethod add-header (connection (stream server-stream) name value)
  (handler-bind ((warning #'muffle-warning))
    (call-next-method)))

(defparameter *tests*
  `()
  "Tests to be executed in the browser on /test page. Each item is test name, test description and test code in Parenscript.")

(defmacro define-server-test (name docstring &body body)
  `(setf *tests*
         (cons `(,,name ,,docstring ,(ps* '((@ tests set) ,name
                                            (lambda () ,@body))))
               (remove ,name *tests* :key 'car :test 'string-equal))))

(defmacro define-simple-server-test (name docstring query-params test)
  "Define a test that makes a request to server with QUERY-PARAMS and then runs
TEST form.

TEST form should evaluate to NIL if test passes, or to a string to print to
table otherwise."
  `(define-server-test ,name
     ,docstring
     (test-request ,name
                   (lambda (reply) ,test)
                   ,@query-params)))

(define-simple-server-test "404"
  "Test that the response of not-found is 404"
  ("GET" "/no-such-page")
  (unless (= 404 (@ reply status)) (@ reply status)))

(define-simple-server-test "Long"
  "Test that long responses are handled well (streams over http streams)."
  ("GET" "/long")
  (let ((resp-code (@ reply status))
        (resp-length (length (@ reply response-text))))
    (unless (and
             (= 200 resp-code)
             (= 2588913 resp-length))
      (+ "code " resp-code ", length "  resp-length))))

(define-simple-server-test "Body passing"
  "Test that body is available to the server."
  ("POST" "/body" "SAMPLE BODY")
  (let ((resp-code (@ reply status))
        (resp-text (@ reply response-text)))
    (unless
        (and
         (= 200 resp-code)
         (= "SAMPLE BODY" resp-text))
      (+ resp-code ", "  resp-text))))

(define-server-test "Event stream"
  "Test event streams - scheduler and locking."
  (let ((source (new (-event-source "/event-stream"))))
    (setf (@ source onmessage)
          (lambda (event)
            (setf (@ ((@ document get-element-by-id) "Event stream") text-content)
                  (@ event data))
            (when (= "5"
                     (@ event last-event-id))
              ((@ source close))
              (set-result "Event stream" "PASS" "ok"))
            (values)))
    (values)))

(define-server-test "1000 events"
  "Test sending 1000 parallel requests. "
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
        ((@ req send))))))

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
    (defparameter tests (new *map))
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
    (constant-handler (out :utf-8 t
                               `((:status "200")
                                 ("content-type" "text/html; charset=utf-8")))
      (with-html-output (out out :prologue "<!DOCTYPE html>")
        (:html
         (:header (:style (esc *test-stylesheet*)))
         (:body :onload "doTests()"
                (:h1 "HTTP/2 demo server test page")
                (:p "Run several tests of the server over Javascript. After few seconds, all yellow rows in the table below should turn green.")
                (print-test-table out)
                (:script
                 (str *js-test-helpers*)
                 ;; NB str is a macro, so no map below
                 (dolist (test (mapcar 'third *tests*))
                   (str test))
                 (str
                  (ps (defun do-tests ()
                        ((@ tests for-each) (lambda (val key map) (val))))))))))))

(defvar *default-certificate-pair*
  '("/tmp/server.key" "/tmp/server.crt")
  "Paths to files with the default private key and certificate to use for server.")

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

(defun run-demo-server (&key (key (car *default-certificate-pair*))
                          (certificate (second *default-certificate-pair*))
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
