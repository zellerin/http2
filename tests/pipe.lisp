;;;; Copyright 2022 by Tomáš Zellerin

(in-package :http2)
;;;; Implement pipes for testing client-server pairs.
;;;; No synchronization, usage in concurrently running processes not expected.

(defclass pipe-end-for-read (binary-stream trivial-gray-streams:fundamental-binary-input-stream)
  ((buffer :accessor get-buffer :initarg :buffer)
   (index  :accessor get-index  :initarg :index)))

(defclass pipe-end-for-write (binary-stream trivial-gray-streams:fundamental-binary-output-stream)
  ((buffer :accessor get-buffer :initarg :buffer)))

(defun make-pipe (&key (buffer-size 4096))
  "Two values, each representing one end of a freshly created one-way binary
pipe: writer and reader."
  (let ((buffer (make-array buffer-size :adjustable t
                                        :fill-pointer 0)))
    (values (make-instance 'pipe-end-for-write
                           :buffer buffer)
            (make-instance 'pipe-end-for-read :buffer buffer
                                              :index 0))))

(defmethod print-object ((pipe pipe-end-for-read) stream)
  (print-unreadable-object (pipe stream :type t)))

(defmethod print-object ((pipe pipe-end-for-write) stream)
  (print-unreadable-object (pipe stream :type t)))

(defun make-full-pipe ()
  "Two values, each representing one end of a full binary pipe: writes to ones are
read from the other."
  (multiple-value-bind (write-stream-a read-stream-a) (make-pipe)
    (multiple-value-bind (write-stream-b read-stream-b) (make-pipe)
      (values
       (make-two-way-stream read-stream-b write-stream-a)
       (make-two-way-stream read-stream-a write-stream-b)))))

(defmethod trivial-gray-streams:stream-read-byte ((stream pipe-end-for-read))
  (if (= (get-index stream) (length (get-buffer stream))) :eof
      (prog1 (aref (get-buffer stream) (get-index stream))
        (incf (get-index stream)))))

(defmethod trivial-gray-streams:stream-listen ((stream pipe-end-for-read))
  (< (get-index stream) (length (get-buffer stream))))

(defmethod trivial-gray-streams:stream-write-byte ((stream pipe-end-for-write) byte)
  (vector-push-extend byte (get-buffer stream)))

(defmethod trivial-gray-streams:stream-listen ((stream pipe-end-for-read))
  "If the index is not on end of stream, it can probably be read."
  ;;  This does not work with clisp, see
  ;;  https://clisp.sourceforge.io/impnotes/non-block-io.html
  (< (get-index stream) (length (get-buffer stream))))

(defun make-dummy-connection (stream &key (class 'logging-connection)
                                       (STREAM-ID 1) (STATE 'open))
  "Make a dummy connection class with one stream of id ID in state STATE. Used for
testing."
  (let ((connection (make-instance class
                         :network-stream stream
                         :last-id-seen stream-id
                         :id-to-use (1+ stream-id))))
    (when state
      (push (make-instance 'logging-stream
                           :stream-id stream-id
                           :state STATE
                           :connection connection)
            (get-streams connection)))
    connection))

(defun process-messages (participants)
  "Let PARTICIPANTS process outstaning messages as long as there are any."
  (loop while participants
        unless
        (loop for participant in participants
              for frame-available = (listen (get-network-stream participant))
              do (add-log participant `(:frame-available ,frame-available))
              when frame-available
                do
                   (handler-case
                       (read-frame participant)
#+nil                     (serious-condition ()
                       (setf participants (remove participant participants))))
                   (return :handled))
        do (return)))

(defmacro with-sender-receiver ((&key (init-state 'open)) &body body)
  "Run BODY with sender and receiver bound to a piped streams.

Bind PROCESS-MESSAGES function to pass messages in both directions until all is
quiet."
  `(multiple-value-bind (write-stream read-stream) (make-full-pipe)
     (let ((sender (make-dummy-connection write-stream))
           (receiver (make-dummy-connection read-stream :state ,init-state))
           (sender-signalled)
           (receiver-signalled))
       (flet ((process-messages ()
                (loop
                  (cond ((and (null receiver-signalled)
                              (listen (get-network-stream receiver)))
                         (handler-case (read-frame receiver)
                           (serious-condition (e) (setf receiver-signalled e))))
                        ((and (null sender-signalled)
                              (listen (get-network-stream sender)))
                         (handler-case (read-frame sender)
                           (serious-condition (e) (setf sender-signalled e))))
                        (t (return))))))
         ,@body))))

(defun check-history (fn pars expected got type)
  (fiasco:is (equalp expected got)
      "(~a ~{~s~^ ~}): ~a should have log~%+++ ~s, not~%--- ~s~%" fn pars type
      expected got))

(defun test-one-frame (send-fn send-pars &key expected-log-connection expected-log-stream
                                           expected-log-sender
                                           expected-sender-error
                                           expected-receiver-error
                                           (stream 1)
                                           (init-state 'open))
      "Send message by SEND-FN with SEND-PARS to receiver, and let all relevant
communication happen. Check that in the end, logs on sender and receiver are as
expected."
  (handler-bind ((warning #'muffle-warning)) ; yes, we know some things are not handled.
    (with-sender-receiver (:init-state init-state)
      (apply send-fn
             (cond
               ((numberp stream)
                (make-instance 'http2-stream :stream-id stream
                                             :connection sender
                                             :network-stream (get-network-stream sender)
                                             :window-size (get-initial-peer-window-size sender)))
               ((eq stream :connection) sender)
               (t (error "Stream parameter must be stream id number or :connection")))
             send-pars)
      (process-messages)
      (fiasco:is (eq expected-sender-error (and sender-signalled (type-of sender-signalled)))
          "Sender error should be ~s is ~s" expected-sender-error sender-signalled)
      (fiasco:is (eq expected-receiver-error
                     (and receiver-signalled (type-of receiver-signalled)))
          "Receiver error should be ~s is ~s" expected-receiver-error receiver-signalled)
      (when (and expected-log-stream) init-state
        (check-history send-fn send-pars expected-log-stream
                       (get-history (car (get-streams receiver))) "stream"))
      (check-history send-fn send-pars expected-log-connection
                     (get-history receiver) "connection")
      (check-history send-fn send-pars expected-log-sender
                     (get-history sender) "sender"))))
