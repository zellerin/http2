;;;; Copyright 2022, 2024, 2026 by Tomáš Zellerin

(in-package :http2/tests/frames)

;; I do not want to make separate package for tests. I test too many internals.
(fiasco:defsuite
  (fiasco-suites::http2/core :bind-to-package #:http2 :in
                        http2/tests::http2/tests))

(fiasco:deftest flags-to-code/test ()
  "Check that flag PADDED is translated correctly."
  (fiasco:is (equalp (flags-to-code `(:padded nil)) 0))
  (fiasco:is (equalp (flags-to-code `(:padded t)) 8)))

(defun brute-octetize (vector)
  "If the parameter is a simple vector, replace it with an octet vector.
This is not efficient, but it simplifies writing some tests."
  (if (typep vector 'simple-vector) (make-initialized-octet-buffer vector) vector))

(defun test-one-frame (fn call-pars &key (init-state 'open) expected-error &allow-other-keys)
  "Apply FN on a dummy stream and PARS, take the octets it produces, and let them
parsed by a similar dummy stream."
  (with-dummy-stream (read-stream :state init-state)
    (with-dummy-stream (stream)
      (apply fn stream (mapcar 'brute-octetize call-pars))
      (setf (http2/core::get-last-id-seen (get-connection read-stream)) 42)
        ;; Check that parsing provides correct error.

      (handler-case
          (progn
            (process-frames (get-connection read-stream) (get-to-write stream))
            (fiasco:is (null expected-error)))
        (http2/core::http2-condition (e)
          (fiasco:is (typep e expected-error)))))))

(defun test-one-frame-connection (fn call-pars &key expected-error &allow-other-keys)
  "Apply FN on a dummy connection and PARS, take the octets it produces, and let them
parsed by a another dummy connection."
  (let ((connection (make-instance 'dummy-connection))
        (read-connection (make-instance 'dummy-connection)))
    (apply fn connection (mapcar 'brute-octetize call-pars))
    ;; Check that parsing provides correct error.
    (handler-case
        (progn
          (process-frames read-connection (get-to-write connection))
          (fiasco:is (null expected-error)))
      (http2-condition (e)
        (fiasco:is (typep e expected-error))))))

(macrolet ((test-a-frame (sub fn &rest args)
             "Define a test that calls FN on a dummy stream and pars, and "
             `(fiasco:deftest ,(intern (format nil "TEST-~a~@[-~a~]" (second fn) sub)) ()
                (test-one-frame ,fn ,@args)))
           (test-connection (sub fn &rest args)
             "Define a test that calls FN on a dummy stream and pars, and "
             `(fiasco:deftest ,(intern (format nil "TEST-~a~@[-~a~]" (second fn) sub)) ()
                (test-one-frame-connection ,fn ,@args))))


  (test-a-frame simple #'write-data-frame '(#(1 2 3 4 5))
                :expected-log-stream '((:PAYLOAD #(1 2 3 4 5))))

  (test-a-frame from-closed #'write-data-frame '(#(1 2 3 4 5))
                :init-state 'closed
                :expected-log-stream nil
                :expected-log-sender '((:GO-AWAY :LAST-STREAM-ID 0 :ERROR-CODE +STREAM-CLOSED+))
                :expected-error 'http2/core::bad-stream-state)

  (test-a-frame padded #'write-data-frame '(#(1 2 3 4 5)
                                            :padded #(0 1 2 3  6 7))
                :expected-log-stream '((:PAYLOAD #(1 2 3 4 5))))

  (test-a-frame strings #'write-headers-frame
                '(#(0 130 148 231 3 98 97 114 132 0 3 98 97 122 3 98 97 104)
                  :end-headers t)
                :expected-log-stream
                '((:header "foo" "bar")
                  (:header :path "/")
                  (:header "baz" "bah")
                  (:end-headers)))

  (test-a-frame header+prio #'write-headers-frame
                `(#(0 130 148 231 3 98 97 114 132 0 3 98 97 122 3 98 97 104)
                  :end-headers t
                  :priority ,(make-priority :exclusive t :stream-dependency 12
                                            :weight 34))
                :expected-log-stream
                '((:new-prio :exclusive t :weight 34 :dependency 12)
                  (:header "foo" "bar")
                  (:header :path "/")
                  (:header "baz" "bah")
                  (:end-headers)))

;;;; this does not fit the test frame at the moment.
  #+nil(test-a-frame #'write-headers-frame
                     '((("foo" "bar")
                        (:path "/")
                        ("baz" "bah")))
                     :expected-log-stream
                     '((:header "foo" "bar")
                       (:header :path "/")
                       (:header "baz" "bah"))
                     :stream 2 :init-state nil
                     :expected-log-connection '((:new-stream-requested 1 1)))

  (test-a-frame exclusive #'write-priority-frame
                (list (make-priority :exclusive t :stream-dependency 12
                                     :weight 34))
                :expected-log-stream
                '((:new-prio :exclusive t :weight 34 :dependency 12)))

  (test-a-frame non-exclusive #'write-priority-frame
                (list (make-priority :exclusive nil :stream-dependency 12
                                     :weight 34))
                :expected-log-stream
                '((:new-prio :exclusive nil :weight 34 :dependency 12)))

  (test-a-frame undefined-code #'write-rst-stream-frame '(#xdeadbeef)
                :expected-error 'http2/core::http-stream-error-received
                :expected-log-stream nil) ; closed stream

  (test-a-frame protocol-error #'write-rst-stream-frame '(1)
                :expected-error 'http2/core::http-stream-error-received
                :expected-log-stream nil) ; closed stream

  (test-connection nil #'write-ping-frame '(#x42)
                :expected-log-connection '((:ping #x42))
                :expected-log-sender '((:pong #x42))
                :stream :connection)

  (test-connection undefined-code #'write-goaway-frame `(#x42 #xec0de #(1 2 3 4 5))
                   :expected-error 'http2/core::go-away
                   :expected-log-connection
                '((:go-away :last-stream-id #x42 :error-code
                   undefined-error-code-ec0de))
                :stream :connection)

  (test-connection no-error #'write-goaway-frame `(#x42 ,http2/core::+no-error+ #(1 2 3 4 5))
                :expected-error 'http2/core::go-away-no-error
                :expected-log-connection
                '((:go-away :last-stream-id #x42
                            :error-code +no-error+))
                :stream :connection)

  (test-a-frame stream #'write-window-update-frame '(#x40000)
                :expected-log-stream
                '((:window-size-increment #xffff + #x40000)))

  (test-a-frame connection #'write-window-update-frame '(#x40000)
                :stream :connection
                :expected-log-connection
                '((:window-size-increment #xffff + #x40000))))

(defmacro define-protocol-error-test (name error &body body)
  "Define test NAME that runs BODY and expects ERROR."
  `(fiasco:deftest ,name ()
     (let ((err (fiasco:signals go-away
                  ,@body)))
       (fiasco:is (eq ,error (get-error-code err)))
       err)))

#+nil(defun read-intro (sender receiver)
  (read-client-preface receiver)
  (read-frame receiver)            ; settings
  (read-frame sender)              ; settings
  (read-frame receiver)            ; ACK
  (read-frame sender))              ; ACK  )

#+nil(defmacro with-test-client-to-server-setup (&body body)
  "Run BODY with server and receiver bound to client, respective server connection
that are connected and initialized."
  `(multiple-value-bind (write-stream read-stream) (make-full-pipe)
     (declare (special write-stream))
     (let ((sender (make-instance 'vanilla-client-connection
                                  :network-stream (make-synonym-stream 'write-stream)))
           (receiver (make-instance 'vanilla-server-connection
                                    :network-stream read-stream)))
       (read-intro sender receiver)
       ,@body)))

#+nil(fiasco:deftest with-test-client-to-server-setup/test ()
#+nil  (with-test-client-to-server-setup
    (fiasco:is (null (listen read-stream)))
    (fiasco:is (null (listen write-stream)))))

(defmethod get-stream-id ((id fixnum))
  id)

(defmethod get-connection ((id fixnum))
  :dummy)

(defvar *dummy-write-connection* (make-octet-buffer 4096))
(defvar *dummy-idx* 0)

(defmethod queue-frame ((conn (eql :dummy)) frame)
  (replace *dummy-write-connection* frame :start1 *dummy-idx*)
  (incf *dummy-idx* (length frame)))

(defun read-frame-from-octets (connection octets start end)
  "Read one frame related to the CONNECTION from STREAM. Flush outstanding data to
write, read the header and process it."
  (let ()
    (when (< end (+ 9 start))
      (error 'end-of-file :stream connection))
    (multiple-value-bind (receive-fn length)
        (parse-frame-header connection octets start (+ 9 start))
      (declare (compiled-function receive-fn)
               ((unsigned-byte 24) length))
      (incf start 9)
      (loop while (not (equal #'parse-frame-header receive-fn))
            do
               (when (< end (+ start length))
                 (error 'end-of-file :stream connection))
               (multiple-value-setq (receive-fn length)
                 (funcall receive-fn connection octets start (incf start length)))))
    start))

(defun read-frames-from-octets (connection octets start end)
  "Read one frame related to the CONNECTION from OCTETS. Flush outstanding data to
write, read the header and process it."
  (let ()
    (when (< end (+ 9 start))
      (error 'end-of-file :stream connection))
    (multiple-value-bind (receive-fn length)
        (parse-frame-header connection octets start (+ 9 start))
      (declare (compiled-function receive-fn)
               ((unsigned-byte 24) length))
      (incf start 9)
      (loop while  (>= end (+ start length))
            do
               (multiple-value-setq (receive-fn length)
                 (funcall receive-fn connection octets start (+ start length)))))
    start))

(fiasco:deftest test-continuation-header ()
  "Let us take a header and try to split it on all possible places.

And then, try with two splits."
#+fixme
  (loop with headers =  (compile-headers (http2/client::request-headers "GET" "/" "localhost") nil)
        for split-idx from 0 to (length headers)
        for receiver = (make-instance 'dummy-server-connection :stream-class 'server-stream)
        do
           (setf *dummy-idx* 0)
           (write-headers-frame 41
                                (subseq headers 0 split-idx)
                                :end-headers nil)
           (write-continuation-frame 41
                                     (subseq headers split-idx )
                                     :end-headers t)
           (with-simple-restart (skip-case "Skip split at ~a" split-idx)
             (read-frames-from-octets receiver
                                      *dummy-write-connection* 0 *dummy-idx*)
             (let ((received-stream (car (get-streams receiver)) ))
               (fiasco:is
                   (equal "/" (get-path received-stream)))
               (fiasco:is
                   (equal "GET" (get-method received-stream)))
               (fiasco:is
                   (eq 'open (get-state received-stream)))))
           (loop for second-split-idx from split-idx to (length headers)
                 for receiver = (make-instance 'server-http2-connection
                                               :stream-class 'server-stream)
                 do
                    (setf *dummy-idx* 0)
                    (write-headers-frame 41
                                         (subseq headers 0 split-idx)
                                         :end-headers nil)
                    (write-continuation-frame 41
                                              (subseq headers split-idx second-split-idx )
                                              :end-headers nil)
                    (write-continuation-frame 41
                                              (subseq headers second-split-idx)
                                              :end-headers t)
                    (with-simple-restart (skip-split "Skip split at ~a ~a" split-idx second-split-idx)
                      (read-frame-from-octets receiver *dummy-write-connection* 0 *dummy-idx*))
                    (let ((received-stream (car (get-streams receiver)) ))
                      (fiasco:is
                          (equal "/" (get-path received-stream)))
                      (fiasco:is
                          (equal "GET" (get-method received-stream)))
                      (fiasco:is
                          (eq 'open (get-state received-stream)))))))
