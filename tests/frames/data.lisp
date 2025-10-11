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
