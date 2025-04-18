;;;; Copyright 2022, 2024 by Tomáš Zellerin

(in-package :http2)

;;;; FIXME: move it where it belongs
(defsection to-write-buffer
    (:title "To-write buffer")
  (queue-to-send-to-peer function)
  (vector-to-write function))

(defun queue-to-send-to-peer (connection buffer)
  "Add new data to the TO-WRITE buffer of the connection"
  (with-slots (to-write) connection
    (setf to-write (if to-write (cons to-write buffer) buffer))))

(defun map-tree (tree atom-fn cons-fn)
  (if (consp tree)
    (funcall cons-fn
             (map-tree (car tree) atom-fn cons-fn)
             (map-tree (cdr tree) atom-fn cons-fn))
    (funcall atom-fn tree)))

; (concatenate-to-write '(#(1 2 3) (#(3 4 5) #(13 14 15) #(10)))) ->
; #(1 2 3 3 4 5 13 14 15 10)
(defun concatenate-to-write (to-write)
  "Concatenated vector to write."
  (let ((buffer (make-octet-buffer (map-tree to-write 'length #'+)))
        (idx 0))
    (map-tree to-write
              (lambda (chunk)
                (replace buffer chunk :start1 idx)
                (incf idx (length chunk)))
              (constantly nil))
    buffer))

(defun vector-to-write (connection)
  (concatenate-to-write (get-to-write connection)))

(defmacro with-vector-to-write ((connection) &body body)
  `(progn
     (assert (null get-to-write ,connection))
     (unwind-protect
          (progn ,@body)
       (setf (get-to-write ,connection) nil))))


(defun make-dummy-connection (&key (class 'logging-connection)
                                       (STREAM-ID 1) (STATE 'open))
  "Make a dummy connection class with one stream of id ID in state STATE. Used for
testing."
  (let ((connection (make-instance class
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
                       (progn
                         (read-frame participant)
                         (write-sequences (get-network-stream participant)
                                          (get-to-write participant)))
#+nil                     (serious-condition ()
                       (setf participants (remove participant participants))))

                   (return :handled))
        do (return)))

(defmacro with-sender-receiver ((&key (init-state 'open)) &body body)
  "Run BODY with sender and receiver bound to a piped streams.

Bind PROCESS-MESSAGES function to pass messages in both directions until all is
quiet."
  `(let ((sender (make-dummy-connection))
         (receiver (make-dummy-connection :state ,init-state))
         (sender-signalled)
         (receiver-signalled))
     ,@body))

(defun check-history (fn pars expected got type)
  (fiasco:is (equalp expected got)
      "(~a ~{~s~^ ~}): ~a should have log~%+++ ~s, not~%--- ~s~%" fn pars type
      expected got))

(defun make-dummy-stream-from-stream-id (connection stream-id)
  (cond
    ((numberp stream-id)
     (make-instance 'http2-stream :stream-id stream-id
                                  :connection connection
                                  :window-size (get-initial-peer-window-size sender)))
    ((eq stream-id :connection) connection)
    (t (error "Stream parameter must be stream id number or :connection"))))

;; TODO: move somewhere. Some canonical name?
(defun map-reduce-tree (fn tree init-value)
  (cond
    ((null tree) init-value)
    ((and (vectorp tree) (zerop (length tree)) init-value))
    ((atom tree) (funcall fn tree init-value))
    ((consp tree)
     (map-reduce-tree fn (cdr tree) (map-reduce-tree fn (car tree) init-value)))))
;;; FIXME: should not be used anymore
(defun do-updates (connection chunks action size)
  (dolist (chunk chunks)
    (loop with consumed = 0
          while (<= (+ size consumed) (length chunk))
          do (multiple-value-setq (action size)
               (funcall action connection (subseq chunk consumed (incf consumed size))))
          finally (unless (= consumed (length chunk))
                    (error "Bad chunk sizes: leftover ~a from ~a, needed ~a"
                           (- (length chunk) consumed) chunk size))))
  (values action size))

(defun exchange-frames (sender receiver)
  (declare (write-buffer-connection-mixin sender receiver))
  (loop for chunks = (get-to-write sender)
        while (plusp (fill-pointer chunks))
        do
           (with-simple-restart (continue "Stop processing")
             (map nil (lambda (chunk)
                        (multiple-value-bind (parser size)
                            (parse-frame-header receiver chunk 0 9)
                          (when (plusp size)
                            (funcall parser receiver (subseq chunk 9)))))
                  (get-to-write sender)))
           (setf (fill-pointer (get-to-write sender)) 0)
           (rotatef receiver sender)))

(defun test-one-frame (send-fn send-pars
                       &key expected-log-connection expected-log-stream
                         expected-log-sender
                         (expected-error 'null)
                         (stream 1)
                         (init-state 'open))
  "Send message by SEND-FN with SEND-PARS to receiver, and let all relevant
communication happen. Check that in the end, logs on sender and receiver are as
expected."
  ;; FIXME: 2024 doc update
  ;; this should be now the simplest of the frame handlers (is it a term?).
  ;; Now it is over complicated
  (with-simple-restart (close-connection "Close connection")
    (handler-bind ((warning #'muffle-warning)) ; yes, we know some things are not handled.
      (let ((sender (make-dummy-connection))
            (receiver (make-dummy-connection :state init-state))
            signalled)
        (apply send-fn
               (make-dummy-stream-from-stream-id sender stream)
               send-pars)

        (handler-bind
            ((stream-error (lambda (e)
                             (setf signalled e)
                             (continue)))
             (connection-error (lambda (e)
                                 (setf signalled e)
                                 (continue))))
            (exchange-frames sender receiver)
          ;; 20240712 TODO: make common parent mixin
)

        (fiasco:is (typep signalled expected-error)
               "Expected error should be ~s is ~s" expected-error signalled)
        (when (and expected-log-stream) init-state
              (check-history send-fn send-pars expected-log-stream
                             (get-history (car (get-streams receiver))) "stream"))
        (check-history send-fn send-pars expected-log-connection
                       (get-history receiver) "connection")
        (check-history send-fn send-pars expected-log-sender
                       (get-history sender) "sender")))))
