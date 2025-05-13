(ql:quickload 'clip-1994/doc)
(ql:quickload 'http2/server)
(ql:quickload 'cl-who)

(mgl-pax:define-package http-experiments
    (:use #:cl #:clip #:http2/client #:http2/core
          #:mgl-pax #:clip/doc
          #:http2/server))

(in-package http-experiments)

(defsection @index
    (:title "Experiments with poll server and openssl"))

;; change for readable values
(defmethod clip::standard-value-printer ((value t) stream)
  (prin1 value stream))

(defclip real-time ()
  "Real time in ms since enabling or reset of the clip"
  (:enable-function (setf *last-real-time* (get-internal-real-time))
   :reset-function  (setf *last-real-time* (get-internal-real-time))
   :disable-function t)
  (round (- (get-internal-real-time) *last-real-time*)
         internal-time-units-per-second))

(define-simulator collect-and-encrypt-data
  :description
  "Send data to a poll client instance, and let it encrypt."
;  :system-name ""
  :start-system (multiple-value-bind (srv-socket client-socket)
                    (sb-posix:pipe)
                  (let ((server (http2/server/poll::make-client-object
                                 srv-socket
                                 (http2/server/poll::make-http2-tls-context (make-instance 'http2/server::certificated-dispatcher))
                                 (http2/openssl:bio-s-mem)))
                        (client )))))

(define-experiment h2load-on-url (url-list n-m-c-list version)
  :simulator h2load-test
  :system-version (identity version)
  :variables ((url in url-list)
              (iterations-threads-clients in n-m-c-list))

  :instrumentation (h2load-req/s warnings real-time)
  :after-trial (write-current-experiment-data))
