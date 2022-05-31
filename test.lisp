(in-package http2)

(defun do-test ()
  (multiple-value-bind (body headers)
      (retrieve-url "https://example.com")
    (assert (search "<title>Example Domain</title>" body))
    (assert (assoc :status headers))))
