(in-package http2)


(fiasco:deftest test-frames ()

  (test-one-frame #'write-data-frame '(#(1 2 3 4 5))
                  :expected-log-stream '((:PAYLOAD #(1 2 3 4 5))))

  (test-one-frame #'write-data-frame '(#(1 2 3 4 5))
                  :init-state 'closed
                  :expected-sender-error 'go-away
                  :expected-receiver-error 'peer-should-go-away
                  :expected-log-sender '((:GO-AWAY :LAST-STREAM-ID 5 :ERROR-CODE +NO-ERROR+)))

  (test-one-frame #'write-data-frame '(#(1 2 3 4 5)
                                       :padded #(0 1 2 3  6 7))
                  :expected-log-stream '((:PAYLOAD #(1 2 3 4 5))))

  ;; binary inputs
  (test-one-frame #'write-headers-frame
                  (list (list (encode-header "foo" "bar")
                              (encode-header :path "/")
                              (encode-header "baz" "bah")))
                  :expected-log-stream
                  '((:header "foo" "bar")
                    (:header :path "/")
                    (:header "baz" "bah")))

  ;; string inputs
  (test-one-frame #'write-headers-frame
                  '((("foo" "bar")
                     (:path "/")
                     ("baz" "bah"))
                    :end-headers t)
                  :expected-log-stream
                  '((:end-headers)
                    (:header "foo" "bar")
                    (:header :path "/")
                    (:header "baz" "bah")))

    (test-one-frame #'write-headers-frame
                  '((("foo" "bar")
                     (:path "/")
                     ("baz" "bah")))
                  :expected-log-stream
                  '((:header "foo" "bar")
                    (:header :path "/")
                    (:header "baz" "bah")))

  ;;;; this does not fit the test frame at the moment.
  #+nil  (test-one-frame #'write-headers-frame
                  '((("foo" "bar")
                     (:path "/")
                     ("baz" "bah")))
                  :expected-log-stream
                  '((:header "foo" "bar")
                    (:header :path "/")
                    (:header "baz" "bah"))
                  :stream 2 :init-state nil
                  :expected-log-connection '((:new-stream-requested 1 1)))

  (test-one-frame #'write-priority-frame '(t 12 34)
                  :expected-log-stream
                  '((:new-prio :exclusive t :weight 34 :dependency 12)))

  (test-one-frame #'write-priority-frame '(nil 12 34)
                  :expected-log-stream
                  '((:new-prio :exclusive nil :weight 34 :dependency 12)))

  (test-one-frame #'write-rst-stream-frame '(#xdeadbeef)
                  :expected-log-stream
                  '((:closed :error undefined-error-code-deadbeef)))

  (test-one-frame #'write-rst-stream-frame '(1)
                  :expected-log-stream
                  '((:closed :error +protocol-error+)))

  (test-one-frame 'write-settings-frame '(((1 . 2)))
                  :expected-log-connection '((:setting :header-table-size 2)
                                             (:settings-ACK-NEEDED))
                  :stream :connection
                  :expected-log-sender '((:settings-ACKed)))

  #+nil(test-one-frame 'write-push-promise-frame )

  (test-one-frame #'write-ping-frame '(#x42)
                  :expected-log-connection '((:ping #x42))
                  :expected-log-sender '((:pong #x42))
                  :stream :connection)

  (test-one-frame #'write-goaway-frame `(#x42 #xec0de #(1 2 3 4 5) nil)
                  :expected-log-connection
                  '((:go-away :last-stream-id 966878 :error-code
                     undefined-error-code-42))
                  :stream :connection
                  :expected-receiver-error 'go-away)

  (test-one-frame #'write-goaway-frame `(,+no-error+ #xec0de #(1 2 3 4 5) nil)
                  :expected-log-connection
                  '((:go-away :last-stream-id 966878
                              :error-code +no-error+
                              ))
                  :stream :connection
                  :expected-receiver-error 'go-away)

  (test-one-frame #'write-window-update-frame '(#x40000 nil)
                  :expected-log-stream
                  '((:window-size-increment #x40000)))

  (test-one-frame #'write-window-update-frame '(#x40000 nil)
                  :stream :connection
                  :expected-log-connection
                  '((:window-size-increment #x40000))))

(defvar *test-webs*
          '(("https://example.com" "<title>Example Domain</title>" 200)
            ("https://lupa.cz" "Moved Permanently" "301")
            ("https://www.lupa.cz" "Lupa" 200)
            ("https://www.seznam.cz" "" 200))
  "List of tripples for testing pages: URL, text on page and status code.")

#+nil(defun test-webs (&optional (webs *test-webs*))
  (loop for (page search-term code) in webs
        do
           (multiple-value-bind (body headers)
               (http2/client::retrieve-url page)
             (fiasco:is (search search-term body)
                 "Page ~a does not contain ~a" page search-term)
             (fiasco:is (equal code (cdr (assoc :status headers)))
                 "Page ~a does not have status ~a" page code)))  )

(defun do-test (&optional remote-tests)
  (test-frames))
