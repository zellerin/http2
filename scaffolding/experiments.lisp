(ql:quickload 'clip-1994)
(mgl-pax:define-package http-experiments
    (:use #:cl #:clip #:http2/client #:http2/server-example #:http2))

(in-package http-experiments)

(defvar *result* nil)
(defclip result-values () (:components (text-size code response-headers))
  (destructuring-bind (text code response-headers &rest more) *result*
    (declare (ignore more))
    (values) (list (length text)) code response-headers))

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
  (mapcar 'prin1-to-string (remove-duplicates (mapcar 'princ-to-string  *warnings*) :test 'equal)))

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
