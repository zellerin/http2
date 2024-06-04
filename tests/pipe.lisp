;;;; Copyright 2022 by Tomáš Zellerin

(in-package :http2)

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
