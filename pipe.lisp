(in-package http2)
;;;; Implement pipes for testing client-server pairs.
;;;; No synchronization, usage in concurrently running processes not expected.

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
  "Two ends of a freshly created one-way binary pipe: writer and reader."
  (let ((buffer (make-array buffer-size :adjustable t
                                        :fill-pointer 0)))
    (values (make-instance 'pipe-end-for-write
                                       :buffer buffer)
            (make-instance 'pipe-end-for-read :buffer buffer
                           :index 0))))

(defun make-full-pipe ()
  "Two end of a full binary pipe: writes to ones are read from the other."
  (multiple-value-bind (write-stream-a read-stream-a) (make-pipe)
    (multiple-value-bind (write-stream-b read-stream-b) (make-pipe)
      (values
       (make-two-way-stream read-stream-b write-stream-a)
       (make-two-way-stream read-stream-a write-stream-b)))))

(defmethod trivial-gray-streams:stream-read-byte ((stream pipe-end-for-read))
  (if (= (get-index stream) (length (get-buffer stream))) :eof
      (prog1 (aref (get-buffer stream) (get-index stream))
        (incf (get-index stream)))))

(defmethod trivial-gray-streams:stream-listen (stream)
  "If the index is not on end of stream, it can probably be read."
  (< (get-index stream) (length (get-buffer stream))))

(defun make-dummy-connection (stream &key (class 'logging-connection)
                                       (STREAM-ID 1) (STATE 'open))
  "Make a dummy connection class with one stream of id ID in state STATE. Used for
testing."
  (make-instance class
                 :network-stream stream
                 :streams (list (make-instance 'logging-stream
                                               :stream-id stream-id
                                               :state STATE))))

(defmacro with-sender-receiver (() &body body)
  "Run BODY with sender and receiver bound to a piped streams.

Bind PROCESS-MESSAGES function to pass messages in both directions until all is
quiet."
  `(multiple-value-bind (write-stream read-stream) (make-full-pipe)
     (let ((sender (make-dummy-connection write-stream))
           (receiver (make-dummy-connection read-stream)))
       (flet ((process-messages ()
                (loop
                  (cond ((listen (get-network-stream receiver))
                         (read-frame receiver))
                        ((listen (get-network-stream sender))
                         (read-frame sender))
                        (t (return))))))
         ,@body))))

(defun check-history (fn pars expected got type)
  (stefil:is (equalp expected got)
      "(~a ~{~s~^ ~}): ~a should have log~%+++ ~s, not~%--- ~s~%" fn pars type
      expected got))

(defun test-one-frame (send-fn send-pars &key expected-log-connection expected-log-stream
                                           expected-log-sender
                                           (stream 1))
  (with-sender-receiver ()
    "Send message by SEND-FN with SEND-PARS to receiver, and let all relevant
communication happen. Check that in the end, logs on sender and receiver are as
expected."
    (apply send-fn sender
           (cond
             ((numberp stream)
              (make-instance 'http2-stream :stream-id 1))
             ((eq stream :connection) sender)
             (t (error "Stream parameter must be stream id number or :connection")))
           send-pars)
    (process-messages)
    (check-history send-fn send-pars expected-log-stream
                   (get-history (car (get-streams receiver))) "stream")
    (check-history send-fn send-pars expected-log-connection
                   (get-history receiver) "connection")
    (check-history send-fn send-pars expected-log-sender
                   (get-history sender) "sender")))
