(in-package http2/core)

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


(defclass test-buffered-stream (http2-stream http2-connection)
  ((snippets        :accessor get-snippets        :initarg :snippets))
  (:default-initargs :snippets nil))

(defmethod write-buffered-frame ((stream test-buffered-stream) buffer offset size end-stream)
  (push (make-array size :initial-contents (subseq buffer offset (+ offset size)))
        (get-snippets stream))
  (when end-stream (push :end-stream (get-snippets stream))))

(defmethod flush-http2-data ((stream test-buffered-stream))
  (push :end-stream (get-snippets stream)))

(defun test-buffered-frame-output (window frame-size process-fn)
  (let ((*default-stream-buffer-size* 20)
        (test-stream (make-instance 'test-buffered-stream :peer-window-size window :max-peer-frame-size frame-size)))
    (declare (dynamic-extent test-stream))
    (setf (get-connection test-stream) test-stream)
    (funcall process-fn test-stream)
    (reverse (mapcar (lambda (s) (if (vectorp s) (length s) s))
                     (get-snippets test-stream)))))

(fiasco:deftest buffered-output/test ()
  "Test that incoming buffered data are properly split to data frames."
  (macrolet ((@ ((peer-window-size max-frame-size) &body body)
                 `(test-buffered-frame-output ,peer-window-size ,max-frame-size
                   (lambda (test-stream) ,@body))))
    (fiasco:is (equalp
                '(4 4 :end-stream 2 :end-stream)
                (@ (20 4)
                   (write-sequence-to-stream test-stream #(1 2 3 4 5 6 7 8 9 10) 0 10)
                   (flush-stream-buffer test-stream nil))))))
