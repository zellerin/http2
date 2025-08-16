(in-package http2/core)

(defclass dummy-connection (http2-connection write-buffer-connection-mixin) ())
(defclass dummy-stream (http2-stream header-collecting-mixin) ()
  (:default-initargs
   :connection (make-instance 'dummy-connection)
   :stream-id 42))

(defmethod is-our-stream-id ((conn dummy-connection) id) (= id 17))

(defun make-dummies (&rest pars)
  (let ((stream (apply 'make-instance 'dummy-stream pars)))
    (setf (get-streams (get-connection stream)) (list stream))
    stream))

(defvar *dummy-stream* (make-dummies))

(defmacro with-dummy-stream ((name &rest pars) &body body)
  `(let ((,name (make-dummies ,@pars)))
    ,@body))

(defun test-write-parse-fn (write-fn expected octets &rest pars)
  "Test that
- WRITE-FN applied on the dummy stream with PARS gives OCTETS.
- OCTETS can be parsed
"
  (with-dummy-stream (stream :state 'open)
    (setf (get-last-id-seen (get-connection stream)) 42)
    (let* ((res (apply write-fn (make-instance 'dummy-stream :state 'open) pars))
           (parsed-header
             (multiple-value-list
              (parse-frame-header
               (get-connection stream)
               res 0 9))))
      (fiasco:is (equalp octets res)
          "Generated octets do not match for~& ~a~&Seen:   ~a~&Wanted: ~a" pars  res octets)
      (fiasco:is (equalp (second parsed-header) (- (length octets) 9))
          "Parsed size does not match")
      (fiasco:is (functionp
           (funcall (first parsed-header) (get-connection stream)
                    (subseq res 9))))
      (fiasco:is (equalp (get-headers stream) (getf expected :headers)))
      stream)))

(fiasco:deftest write-data-frame/test ()
  "Write some data frames and test the content"
  (let ((iota-5  (make-initialized-octet-buffer #(1 2 3 4 5)))
        (padding (make-initialized-octet-buffer #(10 11 12))))
    (handler-bind ((no-payload-action 'muffle-warning))
      (test-write-parse-fn #'write-data-frame
                           nil
                           #(0 0 5 0 0 0 0 0 42 1 2 3 4 5)
                           iota-5)

      (test-write-parse-fn #'write-data-frame
                           nil
                           #(0 0 5 0 1 0 0 0 42 1 2 3 4 5)
                           iota-5
                           :end-stream t)

      (test-write-parse-fn #'write-data-frame
                           nil
                           #(0 0 9 0 8 0 0 0 42 3 1 2 3 4 5 10 11 12)
                           iota-5
                           :padded padding)

      ;; Test empty body
      (test-write-parse-fn #'write-data-frame
                           nil
                           #(0 0 4 0 8 0 0 0 42 3 10 11 12)
                           (make-octet-buffer 0)
                           :padded padding))))

(defvar *custom-headers-code*
  (vector-from-hex-text
   "400a637573746f6d2d6b65790d637573746f6d2d686561646572"))

(fiasco:deftest with-padding-marks/test ()
  "Test that the padding is removed correctly for further processing."
  (let ((start 0)
        (length 8)
        (http2/core::data #(1 2 3 4 5 6 7 8)))
    (http2/core:with-padding-marks (nil (http2/core:flags-to-code '(:padded t)) start end)
      (fiasco:is (= start 1))
      (fiasco:is (= end 7)))))
