(in-package http2/core)

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
