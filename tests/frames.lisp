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
  "Run WRITE-FN on the dummy stream and get the generated octets. Compare them with OCTETS.

Test that
- WRITE-FN applied on the dummy stream with PARS gives OCTETS.
- OCTETS can be parsed back
"
  (with-dummy-stream (stream :state 'open)
    (with-dummy-stream (stream-out :state 'open)
      (let ((connection (get-connection stream-out)))
        (setf (get-last-id-seen (get-connection stream)) 42)
        (apply write-fn stream-out pars)
        (let ((res (get-to-write connection)))
          (fiasco:is (equalp octets res)
              "Generated octets do not match for parameters~& ~a~&Seen:   ~a~&Wanted: ~a" pars
              res
              octets)
          (process-frames (get-connection stream) (make-initialized-octet-buffer res))
          (fiasco:is (equalp (get-headers stream) (getf expected :headers)))
          stream)))))

(fiasco:deftest with-padding-marks/test ()
  "Test that the padding is removed correctly for further processing."
  (let ((start 0)
        (length 8)
        (http2/core::data #(1 2 3 4 5 6 7 8)))
    (http2/core:with-padding-marks (nil (http2/core:flags-to-code '(:padded t)) start end)
      (fiasco:is (= start 1))
      (fiasco:is (= end 7)))))
