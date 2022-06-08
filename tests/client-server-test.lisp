(in-package http2)

(defvar *test-webs*
          '(("https://example.com" "<title>Example Domain</title>" 200)
            ("https://lupa.cz" "Moved Permanently" "301")
            ("https://www.lupa.cz" "Lupa" 200)
            ("https://www.seznam.cz" "" 200))
  "List of tripples for testing pages: URL, text on page and status code.")

(fiasco:deftest test-webs (&optional (webs *test-webs*))
  (loop for (page search-term code) in webs
        do
           (multiple-value-bind (body headers)
               (http2/client::retrieve-url page)
             (fiasco:is (search search-term body)
                 "Page ~a does not contain ~a" page search-term)
             (fiasco:is (equal code (cdr (assoc :status headers)))
                 "Page ~a does not have status ~a" page code))))

(fiasco:deftest test-client-server ()
  (multiple-value-bind (client-stream server-stream) (make-full-pipe)
    ;; order matters: client needs to write client message so that server can read it.
    (let ((client (make-instance 'http2/client::sample-client-connection :network-stream client-stream))
          (server (make-instance 'sample-server-connection :network-stream server-stream))
)
      (send-headers client :new
                    (request-headers "GET"  "/" "localhost")
                           :end-stream t)
      (process-messages (list client server))
      (let ((client-stream (car (get-streams client))))
        (fiasco:is (eq (get-state client-stream) 'closed))
        (fiasco:is (search "Hello World" (get-body client-stream)))))))
