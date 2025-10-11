(in-package http2/core)

(defvar *custom-headers-code*
  (vector-from-hex-text
   "400a637573746f6d2d6b65790d637573746f6d2d686561646572"))

(defun write-header-split (stream data splits &rest args)
  "Write headers in DATA in one header and several continuation frames to STREAM.

SPLITS sets where the data are split.
"
  (loop for (from to) on (append (list 0) splits)
        for fn = 'write-headers-frame then 'write-continuation-frame
        do
           (apply fn stream (subseq data from to) :end-headers (null to) args)))

(fiasco:deftest write-headers-frame/test ()
  (handler-bind ((no-new-header-action 'muffle-warning))
    (test-write-parse-fn #'write-headers-frame
                         '(:HEADERs (("custom-key" . "custom-header")))
                         #(0 0 26 1 0 0 0 0 42 64 10 99 117 115 116 111 109 45
                           107 101 121 13 99 117 115 116 111 109 45 104 101 97
                           100 101 114)
                         *custom-headers-code*)

    (test-write-parse-fn #'write-headers-frame
                         '(:HEADERs (("custom-key" . "custom-header")))
                         (make-initialized-octet-buffer
                          #(0 0 26 1 4 0 0 0 42 64 10 99 117 115 116 111 109 45
                            107 101 121 13 99 117 115 116 111 109 45 104 101 97
                            100 101 114))
                         *custom-headers-code*
                         :end-headers t)

    (test-write-parse-fn #'write-headers-frame
                         '(:HEADERs (("custom-key" . "custom-header")))
                         #(0 0 31 1 13 0 0 0 42 4 64 10 99 117 115 116 111 109 45
                           107 101 121 13 99 117 115 116 111 109 45 104 101 97
                           100 101 114 1 2 3 4)
                         *custom-headers-code*
                         :end-headers t
                         :end-stream t
                         :padded (make-initialized-octet-buffer #(1 2 3 4)))

    (test-write-parse-fn #'write-headers-frame
                         '(:HEADERs (("custom-key" . "custom-header")))
                         #(0 0 36 1 45 0 0 0 42 4 0 0 0 12 27 64 10 99 117 115
                           116 111 109 45 107 101 121 13 99 117 115 116 111 109
                           45 104 101 97 100 101 114 1 2 3 4)
                         *custom-headers-code*
                         :end-headers t
                         :end-stream t
                         :padded (make-initialized-octet-buffer #(1 2 3 4))
                         :priority (make-priority :exclusive nil :stream-dependency 12
                                                  :weight 27))))

(fiasco:deftest write-priority-frame/test ()
  (test-write-parse-fn #'write-priority-frame
                       nil
                       #(0 0 5 2 0 0 0 0 42 0 0 0 12 27)
                       (make-priority :exclusive nil :stream-dependency 12
                                                :weight 27)))

(fiasco:deftest continuation-frames/test ()
  "Split the headers in several ways."
  (flet ((@ (split data)
             (test-write-parse-fn #'write-header-split
              '(:headers (("custom-key" . "custom-header")))
              data
              *custom-headers-code* split)))
    (@ nil #(0 0 26 1 4 0 0 0 42 64 10 99 117 115 116 111 109 45 107 101 121 13
             99 117 115 116 111 109 45 104 101 97 100 101 114))
    (@ '(4)
        #(0 0 4 1 0 0 0 0 42 64 10 99 117 0 0 22 9 4 0 0 0 42
          115 116 111 109 45 107 101 121 13 99 117 115 116 111
          109 45 104 101 97 100 101 114))
    (@ '(0)
        #(0 0 0 1 0 0 0 0 42 0 0 26 9 4 0 0 0 42 64 10 99 117
          115 116 111 109 45 107 101 121 13 99 117 115 116 111
          109 45 104 101 97 100 101 114))
    (@ '(0 0)
        #(0 0 0 1 0 0 0 0 42 0 0 0 9 0 0 0 0 42 0 0 26 9 4 0 0 0
          42 64 10 99 117 115 116 111 109 45 107 101 121 13 99
          117 115 116 111 109 45 104 101 97 100 101 114))))
