(ql:quickload 'clip-1994/doc)
(ql:quickload 'http2/client)
(ql:quickload 'http2/server)
(ql:quickload 'cl-who)

(mgl-pax:define-package http-experiments
    (:use #:cl #:clip #:http2/client #:http2/core
          #:mgl-pax #:clip/doc
          #:http2/server))

(in-package http-experiments)

(defsection @index
    (:title "Experiments with HTTP2")
  "Define several \"experiments\" to test the client and server to some
extent. This uses CLIP package that is not in Quicklisp."
  (client-on-many-targets experiment)
  (test-servers function)
  (@setup section))

;; change for readable values
(defmethod clip::standard-value-printer ((value t) stream)
  (prin1 value stream))

(defsection @setup
    (:title "Setup of tests")
    (run-client simulator)
  (result-values clip)
  (warnings clip)
  (result-text clip)
  (real-time clip))

(defvar *result* nil "Results of a run - text, HTTP code and headers")

(defclip result-values ()
    "Results of the HTTP. A composite of three clips:

- body text length,
- HTTP response code and
- list of response headers."
  (:components (text-size code response-headers))
  (destructuring-bind (&optional text code response-headers &rest more) *result*
    (declare (ignore more))
    (values (length text) code response-headers)))

(defclip result-text ()
  "Body text response of the HTTP call."
  ()
  (destructuring-bind (text &rest more) *result*
    (declare (ignore more))
    text))

(defvar *warnings* nil
  "Collected warnings - and possibly the final error - of the running trial.")

(defmacro with-saved-warnings (&body code)
  "Run CODE with WARNINGS being printed out and saved to *WARNINGS*."
  `(handler-bind
       ((warning (lambda (w)
                   (push w *warnings*)
                   (muffle-warning w))))
    ,@code))

(defclip warnings ()
  "List of collected warnings and errors raised by the client."
  (:reset-function (setf *warnings* nil))
  (mapcar 'prin1-to-string (remove-duplicates *warnings* :test 'equal)))

(defvar *last-real-time*)

(defclip real-time ()
  "Real time in ms since enabling or reset of the clip"
  (:enable-function (setf *last-real-time* (get-internal-real-time))
   :reset-function  (setf *last-real-time* (get-internal-real-time))
   :disable-function t)
  (round (- (get-internal-real-time) *last-real-time*)
         internal-time-units-per-second))

(define-simulator run-client
  :description
  "Run the HTTP/2 client RETRIEVE-URL on URL to be specified in the expertiment.

Stores the results of the call so that RESULT-VALUES, RESULT-TEXT and WARNINGS
clips can be attached.

On error, returns empty string and xs -1 as the result code."
  :system-name "HTTP/2 client"
  :start-system (handler-case
                    (with-saved-warnings
                      (setf *result* (multiple-value-call 'list (http2/client:retrieve-url url))))
                  (error (e)
                    (push e *warnings*)
                    (setf *result* (list "" -1 'n/a))
                    (shutdown-and-run-next-trial))))

(defvar *default-url-list* '("https://www.example.com" "https://www.idnes.cz"
                             "https://www.root.cz" "https://www.google.com"))

(define-experiment client-on-many-targets (&optional (url-list *default-url-list*) version)
  "RETRIEVE-URL from defined list of URLs.

This can be used both to test the client (with known good servers) as well as
the server."
  :simulator run-client
  :system-version (identity version)
  :variables ((url in url-list))
  :instrumentation (result-values warnings real-time)
  :after-trial (write-current-experiment-data))

(define-exact-handler "/1"
    (send-text-handler "/Hello World"))

(define-exact-handler "/2"
  (handler (foo :utf-8 nil)
    (with-open-stream (foo foo)
      (send-headers
       '((:status "200")
         ("content-type" "text/html; charset=utf-8")))
      (format foo "Hello World, this is random: ~a" (random 10)))))

(define-exact-handler "/3"
  (handler (foo :utf-8 nil)
      (send-headers
       '((:status "200")
         ("content-type" "text/html; charset=utf-8")))
    (with-open-stream (foo foo)
        (cl-who:with-html-output (foo)
          (:h1 "Hello World")))))

(define-exact-handler "/4"
    (constant-handler (foo :utf-8 nil
                           '((:status "200")
                             ("content-type" "text/html; charset=utf-8")))
      (cl-who:with-html-output (foo)
        (:h1 "Hello World"))))

(define-simulator h2load-test
  :description
  "Run the h2load test on the URL.

Expects parameters URL, ITERATIONS, THREADS and CLIENTS."

  :system-name "h2load"
  :start-system (handler-case
                    (with-saved-warnings
                      (setf *result*
                            (list
                             (unwind-protect
                                  (uiop:run-program
                                   (destructuring-bind (iterations threads clients)
                                       iterations-threads-clients
                                     `("h2load" ,url
                                                "-n" ,(princ-to-string iterations)
                                                "-m" ,(princ-to-string threads)
                                                "-c" ,(princ-to-string clients)))
                                   :output :string)))))
                  #+nil                  (error (e)
                                           (push e *warnings*)
                                           (setf *result* (list ""))
                                           (shutdown-and-run-next-trial))) )

(defclip h2load-req/s ()
  (:components (min-req/s max-req/s mean-req/s))
  (multiple-value-bind (match val)
      (cl-ppcre:scan-to-strings "req/s[ :]*([0-9\\.]+)[ ]*([0-9\\.]+)[ ]*([0-9\\.]+)" (car *result*))
    (declare (ignore match))
    (unless val (error "Not proper result: ~a" (car *result*)))
    (apply 'values (map 'list 'read-from-string val))))

(define-experiment h2load-on-url (url-list n-m-c-list version)
  :simulator h2load-test
  :system-version (identity version)
  :variables ((url in url-list)
              (iterations-threads-clients in n-m-c-list))

  :instrumentation (h2load-req/s warnings real-time)
  :after-trial (write-current-experiment-data))

;;; e.g., (run-experiment 'h2load-on-url :args '(("https://www.example.com") ((5 1 1)) 'dummy) :output-file "/tmp/ex.clasp")

(defun test-servers (port)
  "For each of tested server classes, open server on a random PORT (could be 0 for
testing to select random one, but for tracking changes is better a fixed one),
and run CLIENT-ON-MANY-TARGETS on its endpoints.

Endpoints are defined to cover some situations:

- / - not found"
  (format t "Test servers~%")
  (let (urls threads)
    (dolist (class '(detached-tls-single-client-dispatcher detached-tls-threaded-dispatcher))
      (multiple-value-bind (thread socket)
          (create-server port class
                         :certificate-file "certs/server.crt"
                         :private-key-file "certs/server.key")
        (push (puri:merge-uris (format nil "~(#~a~)" class) (url-from-socket socket "localhost" t)) urls)
        (push thread threads)))
    (unwind-protect
         (progn
           (run-experiment 'client-on-many-targets
                           :args (list (mapcan (lambda (base-url)
                                                 (mapcar (lambda (a)
                                                           (princ-to-string (puri:merge-uris a base-url)))
                                                         '("/" "/1" "/2" "/3")))
                                               urls)
                                       'dummy)
                           :output-file "/tmp/foo2.clasp")
           (run-experiment 'h2load-on-url :args `(,(mapcar' princ-to-string urls)
                                                  ((20000 1 1) (20000 10 1)) dummy)
                                          :output-file "/tmp/h2load.clasp"))
      (map nil #'bordeaux-threads:destroy-thread threads))))

(ignore-errors(delete-file "/tmp/real-life-targets.clasp"))

(run-experiment 'client-on-many-targets :output-file "/tmp/real-life-targets.clasp")
(maybe-create-certificate "certs/server.key" "certs/server.crt" :system "http2")
(test-servers 0)
