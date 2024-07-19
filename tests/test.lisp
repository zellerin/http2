;;;; Copyright 2022, 2024 by Tomáš Zellerin

(in-package :http2)

;; I do not want to make separate package for tests. I test too many internals.
(fiasco:defsuite
  (fiasco-suites::http2 :bind-to-package #:http2 :in
                        fiasco-suites::all-tests))


(fiasco:deftest flags-to-code/test ()
  "Check that flag PADDED is translated correctly."
  (fiasco:is (equalp (flags-to-code `(:padded nil)) 0))
  (fiasco:is (equalp (flags-to-code `(:padded t)) 8)))

(macrolet ((test-a-frame (sub fn &rest args)
             `(fiasco:deftest ,(intern (format nil "TEST-~a~@[-~a~]" (second fn) sub)) ()
                (test-one-frame ,fn ,@args))))


  (test-a-frame simple #'write-data-frame '(#(1 2 3 4 5))
                :expected-log-stream '((:PAYLOAD #(1 2 3 4 5))))

  (test-a-frame from-closed #'write-data-frame '(#(1 2 3 4 5))
                :init-state 'closed
                :expected-log-stream nil
                :expected-log-sender '((:GO-AWAY :LAST-STREAM-ID 0 :ERROR-CODE +STREAM-CLOSED+))
                :expected-error 'bad-stream-state)

  (test-a-frame padded #'write-data-frame '(#(1 2 3 4 5)
                                            :padded #(0 1 2 3  6 7))
                :expected-log-stream '((:PAYLOAD #(1 2 3 4 5))))

  (test-a-frame strings #'write-headers-frame
                '((#(0 130 148 231 3 98 97 114 132 0 3 98 97 122 3 98 97 104))
                  :end-headers t)
                :expected-log-stream
                '((:header "foo" "bar")
                  (:header :path "/")
                  (:header "baz" "bah")
                  (:end-headers)))

  (test-a-frame header+prio #'write-headers-frame
                `((#(0 130 148 231 3 98 97 114 132 0 3 98 97 122 3 98 97 104))
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
  #+nil  (test-a-frame #'write-headers-frame
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
                :expected-log-stream nil)  ; closed stream

  (test-a-frame protocol-error #'write-rst-stream-frame '(1)
                :expected-log-stream nil)  ; closed stream

  (test-a-frame nil #'write-ping-frame '(#x42)
                :expected-log-connection '((:ping #x42))
                :expected-log-sender '((:pong #x42))
                :stream :connection)

  (test-a-frame undefined-code #'write-goaway-frame `(#x42 #xec0de #(1 2 3 4 5))
                :expected-log-connection
                '((:go-away :last-stream-id #x42 :error-code
                   undefined-error-code-ec0de))
                :stream :connection)

  (test-a-frame no-error #'write-goaway-frame `(#x42 ,+no-error+ #(1 2 3 4 5))
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

(defun read-intro (sender receiver)
  (read-client-preface receiver)
  (read-frame receiver)            ; settings
  (read-frame sender)              ; settings
  (read-frame receiver)            ; ACK
  (read-frame sender))              ; ACK  )

(defmacro with-test-client-to-server-setup (&body body)
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

(fiasco:deftest with-test-client-to-server-setup/test ()
  (with-test-client-to-server-setup
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
               (let* ((frame-content (subseq octets start (incf start length))))
                 (multiple-value-setq (receive-fn length)
                   (funcall receive-fn connection frame-content)))))
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
               (let* ((frame-content (subseq octets start (incf start length))))
                 (multiple-value-setq (receive-fn length)
                   (funcall receive-fn connection frame-content)))))
    start))

(fiasco:deftest test-continuation-header ()
  "Let us take a header and try to split it on all possible places.

And then, try with two splits."
  (loop with headers = (car (http2/hpack:request-headers "GET" "/" "localhost"))
        for split-idx from 0 to (length headers)
        for receiver = (make-instance 'server-http2-connection :stream-class 'server-stream)
        do
           (setf *dummy-idx* 0)
           (write-headers-frame 41
                                (list (subseq headers 0 split-idx))
                                :end-headers nil)
           (write-continuation-frame 41
                                     (list (subseq headers split-idx ))
                                     :end-headers t)
           (read-frames-from-octets receiver
                                   *dummy-write-connection* 0 *dummy-idx*)
           (let ((received-stream (car (get-streams receiver)) ))
             (fiasco:is
                 (equal "/" (get-path received-stream)))
             (fiasco:is
                 (equal "GET" (get-method received-stream)))
             (fiasco:is
                 (eq 'open (get-state received-stream))))
           (loop for second-split-idx from split-idx to (length headers)
                 for receiver = (make-instance 'server-http2-connection
                                               :stream-class 'server-stream)
                 do
                    (setf *dummy-idx* 0)
                    (write-headers-frame 41
                                         (list (subseq headers 0 split-idx))
                                         :end-headers nil)
                    (write-continuation-frame 41
                                              (list (subseq headers split-idx second-split-idx ))
                                              :end-headers nil)
                    (write-continuation-frame 41
                                              (list (subseq headers second-split-idx ))
                                              :end-headers t)
                    (read-frame-from-octets receiver *dummy-write-connection* 0 *dummy-idx*)
                    (let ((received-stream (car (get-streams receiver)) ))
                      (fiasco:is
                          (equal "/" (get-path received-stream)))
                      (fiasco:is
                          (equal "GET" (get-method received-stream)))
                      (fiasco:is
                          (eq 'open (get-state received-stream)))))))
