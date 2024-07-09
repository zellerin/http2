(in-package http2)

(defclass dummy-connection (http2-connection write-buffer-connection-mixin) ())
(defclass dummy-stream (http2-stream history-keeping-object) ()
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
      (funcall (first parsed-header) (get-connection stream)
               (subseq res 9))
      (fiasco:is (equalp expected (get-history stream))))))

(fiasco:deftest write-data-frame/test ()
  (test-write-parse-fn #'write-data-frame
                       '((:payload #(1 2 3 4 5)))
                       #(0 0 5 0 0 0 0 0 42 1 2 3 4 5)
                       #(1 2 3 4 5))

  (test-write-parse-fn #'write-data-frame
                       '((:payload #(1 2 3 4 5))
                         (:state-change open -> half-closed/remote)
                         (:closed-remotely))
                       #(0 0 5 0 1 0 0 0 42 1 2 3 4 5)
                       #(1 2 3 4 5)
                       :end-stream t)

  (test-write-parse-fn #'write-data-frame
                       '((:payload #(1 2 3 4 5)))
                       #(0 0 9 0 8 0 0 0 42 3 1 2 3 4 5 10 11 12)
                       #(1 2 3 4 5)
                       :padded #(10 11 12)))

(fiasco:deftest write-headers-frame/test ()
  (test-write-parse-fn #'write-headers-frame
                       '((:HEADER "custom-key" "custom-header"))
                       #(0 0 26 1 0 0 0 0 42 64 10 99 117 115 116 111 109 45
                         107 101 121 13 99 117 115 116 111 109 45 104 101 97
                         100 101 114)
                       (list (vector-from-hex-text
                              "400a637573746f6d2d6b65790d637573746f6d2d686561646572")))

  (test-write-parse-fn #'write-headers-frame
                       '((:HEADER "custom-key" "custom-header")
                         (:end-headers))
                       #(0 0 26 1 4 0 0 0 42 64 10 99 117 115 116 111 109 45
                         107 101 121 13 99 117 115 116 111 109 45 104 101 97
                         100 101 114)
                       (list (vector-from-hex-text
                              "400a637573746f6d2d6b65790d637573746f6d2d686561646572"))
                       :end-headers t)

  (test-write-parse-fn #'write-headers-frame
                       '((:HEADER "custom-key"
                          "custom-header")
                         (:END-HEADERS)
                         (:STATE-CHANGE OPEN ->
                          HALF-CLOSED/REMOTE)
                         (:CLOSED-REMOTELY))
                       #(0 0 31 1 13 0 0 0 42 4 64 10 99 117 115 116 111 109 45
                         107 101 121 13 99 117 115 116 111 109 45 104 101 97
                         100 101 114 1 2 3 4)
                       (list (vector-from-hex-text
                              "400a637573746f6d2d6b65790d637573746f6d2d686561646572"))
                       :end-headers t
                       :end-stream t
                       :padded #(1 2 3 4))

  (test-write-parse-fn #'write-headers-frame
                       '((:NEW-PRIO :EXCLUSIVE NIL :WEIGHT 27 :DEPENDENCY 12)
                         (:HEADER "custom-key" "custom-header")
                         (:END-HEADERS)
                         (:STATE-CHANGE OPEN -> HALF-CLOSED/REMOTE)
                         (:CLOSED-REMOTELY))
                       #(0 0 36 1 45 0 0 0 42 4 0 0 0 12 27 64 10 99 117 115
                         116 111 109 45 107 101 121 13 99 117 115 116 111 109
                         45 104 101 97 100 101 114 1 2 3 4)
                       (list (vector-from-hex-text
                              "400a637573746f6d2d6b65790d637573746f6d2d686561646572"))
                       :end-headers t
                       :end-stream t
                       :padded #(1 2 3 4)
                       :priority (make-priority :exclusive nil :stream-dependency 12
                                                :weight 27)))

(fiasco:deftest write-priority-frame/test ()
  (test-write-parse-fn #'write-priority-frame
                       '((:NEW-PRIO :EXCLUSIVE NIL :WEIGHT 27 :DEPENDENCY 12))
                       #(0 0 5 2 0 0 0 0 42 0 0 0 12 27)
                       (make-priority :exclusive nil :stream-dependency 12
                                                :weight 27)))
