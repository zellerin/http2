(ql:quickload 'clip-1994)
(ql:quickload 'http2/client)
(ql:quickload 'http2/server)
(mgl-pax:define-package http-experiments
    (:use #:cl #:clip #:http2/client #:http2))

(in-package http-experiments)

;; change for readable values
(defmethod clip::standard-value-printer ((value t) stream)
  (prin1 value stream))

(defvar *result* nil)
(defclip result-values () (:components (text-size code response-headers))
  (destructuring-bind (text code response-headers &rest more) *result*
    (declare (ignore more))
    (values (length text) code response-headers)))

(defclip result-text () ()
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
  "Collect warnings of each trial as a list."
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

(defun current-git-version ()
  (string-trim '(#\Newline) (with-output-to-string (o)  (sb-ext:run-program "/bin/git" '("rev-parse" "HEAD") :wait t :output o))))

(define-simulator run-client
  :system-name "HTTP/2 client"
  :system-version (current-git-version)
  :start-system (setf *result* (handler-case
                                   (with-saved-warnings
                                     (multiple-value-call 'list (http2/client:retrieve-url url)))
                                 (error (e) (push e *warnings*) (list "" -1 nil)))))

(define-experiment client-on-many-targets ()
  "RETRIEVE-URL from several sources. Collect results, warnings and measure real time."
  :simulator run-client
  :variables ((url in '("https://www.example.com" "https://www.idnes.cz"
                        "https://www.root.cz" "https://www.google.com")))
  :instrumentation (result-values warnings real-time)
  :after-trial (write-current-experiment-data))

;;; For the server simulation
(define-exact-handler "/1"
    (send-text-handler "/Hello World"))

(define-exact-handler "/2"
  (handler (foo :utf-8 nil)
    (with-open-stream (foo foo)
      (send-headers
       '((:status "200")
         ("content-type" "text/html; charset=utf-8")))
      (format foo "Hello World, this is random: ~a" (random 10)))))

(define-experiment client-to-our-server (server-url)
  :simulator run-client
  :variables ((url in (mapcar (lambda (a) (princ-to-string  (puri:merge-uris a server-url)))
                              '("/" "/1" "/2"))))
  :instrumentation (result-values warnings real-time result-text)
  :after-trial (write-current-experiment-data))

(defun url-from-socket (socket host tls)
  "Return URL that combines HOST with the port of the SOCKET.

This is to be used as callback fn on an open server for testing it."
  (make-instance 'puri:uri
                 :scheme (if tls :https :http)
                 :port (usocket:get-local-port socket)
                 :host host))

(multiple-value-bind (thread socket)
    (create-server 0 'http2::tls-single-client-dispatcher)
  (unwind-protect t
    (run-experiment 'client-to-our-server :args (list (print (url-from-socket socket "localhost" t)))
                                          :output-file "/tmp/foo2.clasp"))
  (bordeaux-threads:destroy-thread thread))
