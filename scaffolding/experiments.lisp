(ql:quickload 'clip-1994/doc)
(ql:quickload 'http2/client)
(ql:quickload 'http2/server)

(mgl-pax:define-package http-experiments
  (:use #:cl #:clip #:http2/client #:http2
        #:mgl-pax #:clip/doc))

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

(defun test-servers (port)
  "For each of tested server classes, open server on a random PORT (could be 0 for
testing to select random one, but for tracking changes is better a fixed one),
and run CLIENT-ON-MANY-TARGETS on its endpoints.

Endpoints are defined to cover some situations:

- / - not found"
  (format t "Test servers~%")
  (dolist (class '(http2::detached-tls-single-client-dispatcher http2::detached-tls-threaded-dispatcher))
    (multiple-value-bind (thread socket)
        (create-server 0 class
                       :certificate-file "certs/server.crt"
                       :private-key-file "certs/server.key")
      (let ((base-url (url-from-socket socket "localhost" t)))
        (unwind-protect
             (run-experiment 'client-on-many-targets
                             :args (list (mapcar (lambda (a)
                                                   (princ-to-string  (puri:merge-uris a base-url)))
                                                 '("/" "/1" "/2"))
                                         class)
                             :output-file "/tmp/foo2.clasp")
          (bordeaux-threads:destroy-thread thread))))))

(delete-file "/tmp/real-life-targets.clasp")
(run-experiment 'client-on-many-targets :output-file "/tmp/real-life-targets.clasp")
(test-servers)
