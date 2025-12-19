(in-package http2/tests)

(defmethod print-object ((c http2/core::dummy-connection) out)
  (if *print-escape* (call-next-method)
      (princ "Dummy connection" out)))

(defmethod http2/core:get-peer-name ((o http2/core::dummy-connection))
  "dummy peer")

(defvar *correct-responses* (make-hash-table :test 'equalp))

(defun test-log-response-fn (code current)
  (let ((expected (gethash code *correct-responses* 'unset)))
    (cond
       ((equal expected 'unset)
        (warn "Adding new case: ~a" current)
        (setf (gethash code *correct-responses*) current))
       (t (is (equal expected current))))))

(defmacro test-log-response (&body body)
  `(test-log-response-fn ',body
                         (with-output-to-string (*log-stream*)
                       ,@body)))

(deftest test-log-messages ()
  "Catch unexpected changes to log messages."
  (test-log-response
    (http2/core:log-closed-stream
     (make-instance 'http2/client:vanilla-client-stream :connection (make-instance 'http2/core::dummy-connection)
                                                        :stream-id 31
                                                        :path "/"
                                                        :scheme "https"
                                                        :authority "foobar")
     'something-broke))
  (test-log-response
    (http2/server::log-server-connected (make-instance 'http2/core::dummy-connection))))
