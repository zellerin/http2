;;;; Copyright 2022 by Tomáš Zellerin

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
                :expected-receiver-error 'bad-stream-state)

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

  (test-a-frame nil 'write-settings-frame '(((1 . 2)))
                :expected-log-connection '((:setting :header-table-size 2)
                                           (:settings-ACK-NEEDED))
                :stream :connection
                :expected-log-sender '((:settings-ACKed)))

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
       (fiasco:is (eq ',error (get-error-code err)))
       err)))

(defmacro with-test-client-to-server-setup (&body body)
  "Run BODY with server and receiver bound to client, respective server connection
that are connected and initialized."
  `(multiple-value-bind (write-stream read-stream) (make-full-pipe)
     (declare (special write-stream))
     (let ((sender (make-instance 'vanilla-client-connection
                                  :network-stream (make-synonym-stream 'write-stream)))
           (receiver (make-instance 'vanilla-server-connection
                                    :network-stream read-stream)))
       (read-client-preface receiver)
       (read-frame receiver)            ; settings
       (read-frame sender)              ; settings
       (read-frame receiver)            ; ACK
       (read-frame sender)              ; ACK
       ,@body)))

(fiasco:deftest with-test-client-to-server-setup/test ()
  (with-test-client-to-server-setup
    (fiasco:is (null (listen read-stream)))
    (fiasco:is (null (listen write-stream)))))

(fiasco:deftest test-continuation-header ()
  "Let us take a header and try to split it on all possible places.

And then, try with two splits."
  (loop with headers = (car (http2/hpack:request-headers "GET" "/" "localhost"))
        for split-idx from 0 to (length headers)
        do
           (with-test-client-to-server-setup
             (let ((new-stream (create-new-local-stream sender)))
               (write-headers-frame new-stream
                                    (list (subseq headers 0 split-idx))
                                    :end-headers nil)
               (write-continuation-frame new-stream
                                         (list (subseq headers split-idx ))
                                         :end-headers t)
               (read-frame receiver)
               (let ((received-stream (car (get-streams receiver)) ))
                 (fiasco:is
                     (equal "/" (get-path received-stream)))
                 (fiasco:is
                     (equal "GET" (get-method received-stream)))
                 (fiasco:is
                     (eq 'open (get-state received-stream))))))
           (loop for second-split-idx from split-idx to (length headers)
                 do
                    (with-test-client-to-server-setup
                      (let ((new-stream (create-new-local-stream sender)))
                        (write-headers-frame new-stream
                                             (list (subseq headers 0 split-idx))
                                             :end-headers nil)
                        (write-continuation-frame new-stream
                                                  (list (subseq headers split-idx second-split-idx ))
                                                  :end-headers nil)
                        (write-continuation-frame new-stream
                                                  (list (subseq headers second-split-idx ))
                                                  :end-headers t)
                        (read-frame receiver)
                        (let ((received-stream (car (get-streams receiver)) ))
                          (fiasco:is
                              (equal "/" (get-path received-stream)))
                          (fiasco:is
                              (equal "GET" (get-method received-stream)))
                          (fiasco:is
                              (eq 'open (get-state received-stream)))))))))
