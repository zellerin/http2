(in-package #:http2/tests/frames)

(defsection @overview (:title "")
  "Test frame reading and writing functions defined in frames.lisp."
  )

;;;; FIXME: this seems very similar to test.lisp, content-wise.

(defsuite
    (http2/tests/frames :bind-to-package #:http2/tests/frames
                        :in http2/tests::http2/tests))

(defun test-write-parse-fn (write-fn expected octets &rest pars)
  "Run WRITE-FN on the dummy stream and get the generated octets. Compare them with OCTETS.

Test that
- WRITE-FN applied on the dummy stream with PARS gives OCTETS.
- OCTETS can be parsed back
"
  (with-dummy-stream (stream :state 'open)
    (with-dummy-stream (stream-out :state 'open)
      (let ((connection (get-connection stream-out)))
        (setf (http2/core::get-last-id-seen (get-connection stream)) 42)
        (apply write-fn stream-out pars)
        (let ((res (http2/core::get-to-write connection)))
          (fiasco:is (equalp octets res)
              "Generated octets do not match for parameters~& ~a~&Seen:   ~a~&Wanted: ~a" pars
              res
              octets)
          (process-frames (get-connection stream)
                          (make-initialized-octet-buffer res))
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
