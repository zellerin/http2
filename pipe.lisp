(in-package http2)
;;;; Implement pipes for testing client-server pairs.
(defclass binary-stream ()
  ())

(defmethod stream-element-type (binary-stream)
  '(unsigned-byte 8))

(defclass pipe-end-for-read (trivial-gray-streams:fundamental-binary-input-stream binary-stream)
  ((buffer :accessor get-buffer :initarg :buffer)
   (index  :accessor get-index  :initarg :index)))

(defclass pipe-end-for-write (trivial-gray-streams:fundamental-binary-output-stream binary-stream)
  ((buffer :accessor get-buffer :initarg :buffer)))

(defun make-pipe (&key (buffer-size 4096))
  (let ((buffer (make-array buffer-size :adjustable t
                                        :fill-pointer 0)))
    (values (make-instance 'pipe-end-for-write
                                       :buffer buffer)
            (make-instance 'pipe-end-for-read :buffer buffer
                           :index 0))))

(defun make-full-pipe ()
  (multiple-value-bind (write-stream-a read-stream-a) (make-pipe)
    (multiple-value-bind (write-stream-b read-stream-b) (make-pipe)
      (values
       (make-two-way-stream read-stream-b write-stream-a)
       (make-two-way-stream read-stream-a write-stream-b)))))

(defmethod trivial-gray-streams:stream-write-byte ((stream pipe-end-for-write) byte)
  (vector-push-extend byte (get-buffer stream)))

(defmethod trivial-gray-streams:stream-read-byte ((stream pipe-end-for-read))
  (if (= (get-index stream) (length (get-buffer stream))) :eof
      (prog1 (aref (get-buffer stream) (get-index stream))
        (incf (get-index stream)))))

(defmethod trivial-gray-streams:stream-listen (stream)
  (< (get-index stream) (length (get-buffer stream))))

(defclass logging-object ()
  ((reversed-history :accessor get-reversed-history :initarg :reversed-history))
  (:default-initargs :reversed-history nil))

(defclass logging-connection (http2-connection logging-object)
  ())

(defclass logging-stream (http2-stream logging-object)
  ())

(defun add-log (object log-pars)
  (push log-pars (get-reversed-history object)))

(defun get-history (object)
  (reverse (get-reversed-history object)))

(defmethod apply-data-frame ((connection logging-connection) (stream logging-stream)
                             payload)
  (add-log stream `(:payload ,payload)))

(defmethod add-header :before ((stream logging-stream) name value)
  (add-log stream `(:header ,name ,value)))

(defmethod apply-stream-priority ((stream logging-stream) exclusive weight stream-dependency)
  (add-log stream `(:new-prio :exclusive ,exclusive :weight ,weight :dependency ,stream-dependency)))

(defmethod peer-resets-stream ((stream logging-stream) error-code)
  (add-log stream `(:closed :error ,(get-error-name error-code))))

(defmethod set-peer-setting :before ((connection logging-connection) name value)
  (add-log connection `(:setting ,name ,value)))

(defmethod peer-expects-settings-ack :before ((connection logging-connection))
  (add-log connection '(:settings-ack-needed)))

(defmethod do-ping :before ((connection logging-connection) data)
  (add-log connection `(:ping ,data)))

(defmethod do-pong :before ((connection logging-connection) data)
  (add-log connection `(:pong ,data)))


(defun make-dummy-connection (stream &key (class 'logging-connection)
                                       (STREAM-ID 1) (STATE 'open))
  "Make a dummy connection class with one stream of id ID in state STATE."
  (make-instance class
                 :network-stream stream
                 :streams (list (make-instance 'logging-stream
                                               :stream-id stream-id
                                               :state STATE))))

(defmacro with-sender-receiver (() &body body)
  `(multiple-value-bind (write-stream read-stream) (make-full-pipe)
     (let ((sender (make-dummy-connection write-stream))
           (receiver (make-dummy-connection read-stream)))
       ,@body)))

(defun check-history (fn pars expected got type)
    (when expected
      (stefil:is (equalp expected got)
          "(~a ~{~s~^ ~}): ~a should have log~%+++ ~s, not~%--- ~s~%" fn pars type
          expected got)))

(defun test-one-frame (send-fn send-pars &key expected-log-connection expected-log-stream
                                           expected-log-sender
                                           (stream 1))
  (with-sender-receiver ()
    (apply send-fn sender
           (cond
             ((numberp stream)
              (make-instance 'http2-stream :stream-id 1))
             ((eq stream :connection) sender)
             (t (error "Stream parameter must be stream id number or :connection")))
           send-pars)
    (loop
      (cond ((listen (get-network-stream receiver))
             (read-frame receiver))
            ((listen (get-network-stream sender))
             (read-frame sender))
            (t (return))))
    (check-history send-fn send-pars expected-log-stream
                   (get-history (car (get-streams receiver))) "stream")
    (check-history send-fn send-pars expected-log-connection
                   (get-history receiver) "connection")
    (check-history send-fn send-pars expected-log-sender
                   (get-history sender) "sender")))
