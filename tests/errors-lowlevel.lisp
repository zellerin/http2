(in-package :http2/core)

(fiasco:defsuite
    (http2/tests/core :bind-to-package #:http2/core
                 :in http2/tests::http2/tests))

(defun process-data-by-connection (data-name)
  "Parse data sample named by DATA-NAME till the end."
  (process-frames (make-instance 'dummy-server-connection)
                  (funcall (payload-code (gethash data-name *payloads*)) nil)))

(defun test-for-error (payload-name error-name)
  "Run payload identified by PAYLOAD-NAME, let the peer process it and exxpect an error."
  (fiasco:is (typep
              (fiasco:signals http2-condition
                (process-data-by-connection payload-name)
                (fiasco:is nil "Error ~a expected for payload ~a" error-name payload-name))
              error-name)))

(fiasco:deftest low-level-errors ()
  (maphash (lambda (key val)
             (declare (ignore key))
             (funcall (payload-test val)))
           *payloads*))
