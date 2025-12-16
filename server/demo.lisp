(cl:defpackage http2/demo
  (:use #:cl #:http2/server #:cl-who #:parenscript))

(in-package http2/demo)

(define-prefix-handler "/demo/redir" (redirect-handler "/demo/ok"))

(define-exact-handler "/demo/ok"
    (send-text-handler "Redirect was OK"
                       :content-type "text/plain; charset=UTF-8"
                       :additional-headers '(("refresh" "3; url=/demo/"))
                       :gzip nil))

(define-exact-handler "/demo"
    (handler (out :utf-8 nil)
      (send-headers
       `((:status "200")
         ("content-type" "text/html; charset=utf-8")))
      (with-open-stream (out out)
        (with-html-output (out)
          (:h1 "Hello World")
          (:p "This server is for testing http2 protocol implementation")
          (:ul
           (:li (:a :href "/demo/redir" "Redirect test")) " "
           (:li (:a :href "/demo/long" "Long page test")) " "
           (:li (:a :href "/demo/test" "A test page") ""))
          (:form :action "/demo/body" :method "post"
                 (:input :type :submit :name "xxx" :value "POST query test"))
          (:p "UTF8 test: Příliš žluťoučký kůň... 😎")))))

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

  "Rudimental CSS to prettify and colorize the results table in /demo/test.

Padding is needed so that the intermediate yellow text can be longer than PASS
text and table width changes on fly.")

(defparameter *tests*
  `()
  "Tests to be executed in the browser on /demo/test page. Each item is test name, test description and test code in Parenscript.")

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

(define-exact-handler "/demo/test"
    (handler (out :utf-8 nil)
      (send-headers
       `((:status "200")
         ("content-type" "text/html; charset=utf-8")))
      (with-open-stream (out out)
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
                          ((@ tests for-each) (lambda (val key map) (val)))))))))))))

;;;; Tests
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
  ("GET" "/demo/no-such-page")
  (unless (= 404 (@ reply status)) (@ reply status)))

(define-simple-server-test "Long"
  "Test that long responses are handled well (streams over http streams)."
  ("GET" "/demo/long")
  (let ((resp-code (@ reply status))
        (resp-length (length (@ reply response-text))))
    (unless (and
             (= 200 resp-code)
             (= 2594035 resp-length))
      (+ "code " resp-code ", length "  resp-length))))

(define-simple-server-test "Body passing"
  "Test that body is available to the server."
  ("POST" "/demo/body" "SAMPLE BODY")
  (let ((resp-code (@ reply status))
        (resp-text (@ reply response-text)))
    (unless
        (and
         (= 200 resp-code)
         (= "SAMPLE BODY" resp-text))
      (+ resp-code ", "  resp-text))))

(define-server-test "Event stream"
  "Test event streams - scheduler and locking."
  (let ((source (new (-event-source "/demo/event-stream"))))
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
        ((@ req open) "GET" "/demo")
        ((@ req send))))))

(defun compute-stream-window-size (stream)
  (min (http2/core:get-peer-window-size (http2/core::get-connection stream))
       (http2/core:get-peer-window-size stream)))

(defun create-part (from to)
  "String with paragraphs for testing in given range"
  (with-output-to-string (out)
    (cl-who:with-html-output (out)
      (loop for i from from to to
            do
               (cl-who:htm (:p "A paragraph #" (format out "~d" i) "."))))))

(defvar *long-handler-items* 99999 "How many paragraphs are sent by *long* handler")

(defvar *long-handler-chunk-size* 500 "How many chunks are in one action. Supposedly this helps speed, but test needed.")

(define-exact-handler "/demo/long"
    ;; FIXME: put the looping logic to the callbacks
    (http2/server::handler (out :utf-8 nil)
      (send-headers
       `((:status "200") ("content-type" "text/html; charset=utf-8")
         ("refresh" "30; url=/")))
      (with-html-output (out)
        (:h1 "Test long body")
        (let ((i (1+ *long-handler-chunk-size*))
              (chunk (create-part 1 *long-handler-chunk-size*)))
          (labels ((write-chunk ()
                     (write-sequence chunk out)
                     (setf chunk (when (<= i *long-handler-items*)
                                   (create-part i (min *long-handler-items* (incf i *long-handler-chunk-size*)))))
                     (values (if chunk #'write-chunk (lambda ()
                                                       (write-sequence "</body></html>" out)
                                                       (close out)
                                                       (values nil 0)))
                             (length chunk))))
            (http2/core::long-write stream #'write-chunk (length chunk)))))))

(define-exact-handler "/demo/slow"
    (handler (foo :utf-8 nil)
      (with-open-stream (foo foo)
        (send-headers
         '((:status "200")
           ("content-type" "text/html; charset=utf-8")))
        (sleep 1)
        (prin1 "Hello World" foo))))

(define-exact-handler "/demo/body"
    (handler (foo :utf-8 nil)
      (with-open-stream (foo foo)
        (send-headers
         `((:status "200") ("content-type" "text/plain; charset=utf-8")
           ("refresh" "3; url=/demo")))
        (write-sequence (http2/server:http-stream-to-string stream) foo))))

(defun schedule-repeated-fn (connection stream delay period fn)
  "Repeatedly call FN on the CONNECTION and STREAM, until the stream is closed or
an error occurs (likely, end of communication).

FN is expected to send a data frames(s) to the stream."
  (labels ((send-event-and-plan-next ()
             (handler-case
                 (progn
                   (unless (eql (http2/core::get-state stream) 'http2/core::closed)
                     (funcall fn connection stream)
                     (schedule-task http2/server::*scheduler* period
                                    #'send-event-and-plan-next
                                    stream)))
               (end-of-file () nil)
               (http2/utils:communication-error () nil))))
    (schedule-task http2/server::*scheduler* delay
                   #'send-event-and-plan-next 'stream)))

(define-exact-handler "/demo/event-stream"
    (lambda (connection stream)
      "Output series of events. Use -N (--no-buffer) curl parameter to test."
      (send-headers stream `((:status "200") ("content-type" "text/event-stream")))
      (let ((i 0))
        (schedule-repeated-fn connection stream 0.0 1.0
                              (lambda (connection stream)
                                (http2/core:write-data-frame stream
                                                  (trivial-utf-8:string-to-utf-8-bytes
                                                   (with-output-to-string (out)
                                                     (format out "id: ~d~%" (incf i))
                                                     (multiple-value-bind (sec min hr day)
                                                         (decode-universal-time (get-universal-time))
                                                       (format out "data: ~2,'0dT~2,'0d:~2,'0d:~2,'0d~2%" day
                                                               hr min sec)))))
                                (http2/core:flush-http2-data connection))))))
