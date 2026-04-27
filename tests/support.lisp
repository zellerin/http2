(mgl-pax:define-package #:http2/tests/support
  (:use #:cl #:http2/core #:mgl-pax))

(in-package #:http2/tests/support)

(defsection @overview (:title "Tests and experiments support")
  "Specific server and client test setups repeatedly use similar patterns."
  (dummy-connection class)
  (dummy-stream class)
  (with-dummy-stream macro))

(defclass dummy-connection (http2-connection write-buffer-connection-mixin)
  ()
  (:documentation "Dummy connection to use for sending frames or parsing octets. Underlying connection for a dummy stream. Specific properties:

- It considers stream id 17 as one it should not receive (not even or odd as normal ones)
- Last stream id seen is 42. All above is idle, all below closed unless in the list.
"))

(defmethod http2/core::is-our-stream-id ((conn dummy-connection) id) (= id 17))

(defclass dummy-stream (http2-stream header-collecting-mixin body-collecting-mixin)
  ()
  (:default-initargs
   :connection (make-instance 'dummy-connection)
   :stream-id 42)
  (:documentation "Dummy stream to use for sending frames or parsing octets. Specific properties:

- Its stream id is 42. "))

(defmethod get-to-write ((stream dummy-stream))
  "Allow GET-TO-WRITE called on the (dummy) stream"
  (get-to-write (get-connection stream)))

(defun make-dummies (&rest pars)
  (let ((stream (apply 'make-instance 'dummy-stream pars)))
    (setf (get-streams (get-connection stream)) (list stream))
    stream))

(defvar *dummy-stream* (make-dummies))

(defmacro with-dummy-stream ((name &rest pars) &body body)
  "Run BODY with NAME bound to a DUMMY-STREAM on DUMMY-CONNECTION."
  `(let ((,name (make-dummies ,@pars)))
     ,@body))
