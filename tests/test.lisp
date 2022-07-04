;;;; Copyright 2022 by Tomáš Zellerin

(in-package :http2)

;; I do not want to make separate package for tests. I test too many internals.
(fiasco:defsuite
  (fiasco-suites::http2 :bind-to-package #:http2 :in
                        fiasco-suites::all-tests))

(macrolet ((test-a-frame (sub fn &rest args)
             `(fiasco:deftest ,(intern (format nil "TEST-~a~@[-~a~]" (second fn) sub)) ()
                  (test-one-frame ,fn ,@args))))


  (test-a-frame simple #'write-data-frame '(#(1 2 3 4 5))
                  :expected-log-stream '((:PAYLOAD #(1 2 3 4 5))))

  (test-a-frame from-closed #'write-data-frame '(#(1 2 3 4 5))
                  :init-state 'closed
                  :expected-log-stream '((:payload #(1 2 3 4 5)))
                  :expected-log-sender '((:GO-AWAY :LAST-STREAM-ID 5 :ERROR-CODE +NO-ERROR+)))

  (test-a-frame padded #'write-data-frame '(#(1 2 3 4 5)
                                            :padded #(0 1 2 3  6 7))
                :expected-log-stream '((:PAYLOAD #(1 2 3 4 5))))

  ;; binary inputs
  (test-a-frame octets #'write-headers-frame
                  (list (list (encode-header "foo" "bar")
                              (encode-header :path "/")
                              (encode-header "baz" "bah")))
                  :expected-log-stream
                  '((:header "foo" "bar")
                    (:header :path "/")
                    (:header "baz" "bah")))

  ;; string inputs
  (test-a-frame strings #'write-headers-frame
                  '((("foo" "bar")
                     (:path "/")
                     ("baz" "bah"))
                    :end-headers t)
                  :expected-log-stream
                  '((:header "foo" "bar")
                    (:header :path "/")
                    (:header "baz" "bah")
                    (:end-headers)))

  (test-a-frame no-end-headers #'write-headers-frame
                  '((("foo" "bar")
                     (:path "/")
                     ("baz" "bah")))
                  :expected-log-stream
                  '((:header "foo" "bar")
                    (:header :path "/")
                    (:header "baz" "bah")))

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

  (test-a-frame exclusive #'write-priority-frame '(t 12 34)
                  :expected-log-stream
                  '((:new-prio :exclusive t :weight 34 :dependency 12)))

  (test-a-frame non-exclusive #'write-priority-frame '(nil 12 34)
                  :expected-log-stream
                  '((:new-prio :exclusive nil :weight 34 :dependency 12)))

  (test-a-frame unknown-code #'write-rst-stream-frame '(#xdeadbeef)
                  :expected-log-stream
                  '((:closed :error undefined-error-code-deadbeef)
                    (:STATE-CHANGE OPEN HTTP2::-> HTTP2::CLOSED))
                  :expected-receiver-error 'http-stream-error)

  (test-a-frame protocol-error #'write-rst-stream-frame '(1)
                  :expected-log-stream
                  '((:closed :error +protocol-error+)
                    (:STATE-CHANGE OPEN HTTP2::-> HTTP2::CLOSED))
                  :expected-receiver-error 'http-stream-error)

  (test-a-frame nil 'write-settings-frame '(((1 . 2)))
                  :expected-log-connection '((:setting :header-table-size 2)
                                             (:settings-ACK-NEEDED))
                  :stream :connection
                  :expected-log-sender '((:settings-ACKed)))

  #+nil(test-a-frame 'write-push-promise-frame )

  (test-a-frame nil #'write-ping-frame '(#x42)
                  :expected-log-connection '((:ping #x42))
                  :expected-log-sender '((:pong #x42))
                  :stream :connection)

  (test-a-frame undefined-code #'write-goaway-frame `(#x42 #xec0de #(1 2 3 4 5))
                  :expected-log-connection
                  '((:go-away :last-stream-id 966878 :error-code
                     undefined-error-code-42))
                  :stream :connection
                  :expected-receiver-error 'go-away)

  (test-a-frame no-error #'write-goaway-frame `(,+no-error+ #xec0de #(1 2 3 4 5))
                  :expected-log-connection
                  '((:go-away :last-stream-id 966878
                              :error-code +no-error+
                     ))
                  :stream :connection)

  (test-a-frame stream #'write-window-update-frame '(#x40000)
                  :expected-log-stream
                  '((:window-size-increment #x40000)))

  (test-a-frame connection #'write-window-update-frame '(#x40000)
                  :stream :connection
                  :expected-log-connection
                  '((:window-size-increment #x40000))))
