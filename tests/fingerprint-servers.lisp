(defpackage http2/fingerprint*
  (:use #:cl #:clip #:http2/core)
  (:import-from #:http2/client #:test-payload))

(in-package http2/fingerprint*)

(defvar error-symbol)
(defvar debug-text)
(defvar stream-errors nil)

(defclip sample-response ()
  (:components (error-code debug-text)
   :reset-function (setf error-symbol nil debug-text nil))
  (values error-symbol debug-text))

(defclip stream-errors ()
  (:reset-function (setf stream-errors nil))
  stream-errors)

(define-simulator sample-server
  :description "Run the payloads over test server."
  :start-system (multiple-value-setq (error-symbol debug-text)
                  (handler-bind
                      ((http2/core::http-stream-error (lambda (e)
                                                        (push (http2/core::get-code e) stream-errors)
                                                        (muffle-warning)))
                       (warning #'muffle-warning))
                    (test-payload url payload)))
  :system-name "Sample-tester")

(define-experiment fingerprint-servers ()
  :simulator sample-server
  :variables ((url in '("https://www.example.com" "https://www.facebook.com" "https://www.seznam.cz"
                        "https://www.root.cz" "https://www.google.com"))
              (payload in 'http2/core::(even-numbered-stream null-connection-window-update
                                                             too-big-padding null-stream-window-update
                                                             send-data-without-being-asked
                                                             id-one-after-id-3)))
  :after-trial (write-current-experiment-data)
  :instrumentation (sample-response stream-errors))
