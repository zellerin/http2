(in-package http2/client)

(defsection @test-samples-client
    (:title "Send test samples to servers")
  (test-payload function))

(defclass payload-testing-client-stream (vanilla-client-stream)
  ())

(defclass payload-request (simple-request)
  ((payload-name :accessor get-payload-name :initarg :payload-name)))



(defmethod fetch-resource ((connection client-http2-connection)
                           (request payload-request) args)

  (queue-frame connection
               (funcall (get-test-sample-code (get-payload-name request)) connection))
  (call-next-method))

(defmethod peer-ends-http-stream ((stream payload-testing-client-stream))
  (when (null (get-streams (get-connection stream)))
    (signal 'client-done :Ok))
  (terpri))

(defun test-payload (url payload)
  "Run a payload against URL. Return error code symbol (e.g., +protocol-error+),
debug text and last ID."
  (handler-case
      (retrieve-url url
                                 :request-class 'payload-request
                                 :payload-name payload)
    (go-away (e) (values (http2/core::get-error-code e)
                         (map 'string 'code-char (http2/core::get-debug-data e))
                         (http2/core::get-last-stream-id e)))
    (client-done (data) data)))
