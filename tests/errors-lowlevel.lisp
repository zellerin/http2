(in-package :http2/core)

(fiasco:defsuite
    (http2/tests/core :bind-to-package #:http2/core
                 :in http2/tests::http2/tests))

(defmacro with-reciever (&body body)
  `(let ((receiver (make-instance 'http2/server::vanilla-server-connection :network-stream (make-broadcast-stream))))
     ,@body))

(defun process-data-by-connection (data-name)
  "Parse data sample named by DATA-NAME till the end."
  (with-reciever
    (loop with data of-type octet-vector = (funcall (payload-code (gethash data-name *payloads*)) nil)
            with start of-type frame-size = 0 and end of-type frame-size = (length data)
            with fn of-type receiver-fn = #'parse-frame-header
            with size of-type frame-size = 9
            for old-size of-type frame-size = size
            while (> end start)
            do
               (multiple-value-setq (fn size) (funcall fn receiver data start (min end (+ start size))))
               (incf start old-size))))

(defun test-for-error (payload-name error-name)
  (declare (optimize debug))
  (handler-case
      (progn
        (process-data-by-connection payload-name)
        (fiasco:is nil "Error ~a expected for payload ~a" error-name payload-name))
    (condition (e)
      (fiasco:is (equal (type-of e) error-name)
          "Error ~a was expected, but got ~a instead" error-name e)
      e)))

(fiasco:deftest low-level-errors ()
  (maphash (lambda (key val)
             (declare (ignore key))
             (funcall (payload-test val)))
           *payloads*))
