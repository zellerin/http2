(in-package http2/core)
;;;; Interface


(defsection @streams
    (:title "Streams and connections")
  (get-streams generic-function)
  (get-connection generic-function)
  (stream-collection class)
  (http2-stream-minimal class))

(defgeneric get-streams (connection)
)

(export '(state connection))

(defclass http2-stream-minimal (flow-control-mixin)
  ((connection       :accessor get-connection       :initarg :connection)
   (stream-id        :accessor get-stream-id        :initarg :stream-id
                     :type stream-id)
   (state            :accessor get-state            :initarg :state
                     :type http2-stream-state))
  (:default-initargs :window-size 0
                     :state 'idle)
  (:documentation
   "Part of representation of HTTP/2 stream needed to read and write frames."))

(defclass stream-collection ()
  ((streams                  :accessor get-streams                  :initarg :streams
                             :documentation
                             "Sequence of streams the connection knows about.
This includes all open and half-closed streams, but not the already closed and
idle streams.")
   (id-to-use                :accessor get-id-to-use                :initarg :id-to-use
                             :type stream-id)
   (last-id-seen             :accessor get-last-id-seen             :initarg :last-id-seen
                             :type stream-id)
   (stream-class             :accessor get-stream-class             :initarg :stream-class
                             :documentation "Class for new streams"))
  (:default-initargs :id-to-use 1
                     :last-id-seen 0
                     :streams nil
                     :window-size 65535
                     :initial-peer-window-size 65535
                     :initial-window-size 65535))

(defsection @stream-states
    (:title "Stream states")

  "
The lifecycle of a stream is shown below.
```
                        +--------+
                send PP |        | recv PP
               ,--------|  idle  |--------.
              /         |        |         \
             v          +--------+          v
         +----------+          |           +----------+
         |          |          | send H /  |          |
  ,------| reserved |          | recv H    | reserved |------.
  |      | (local)  |          |           | (remote) |      |
  |      +----------+          v           +----------+      |
  |          |             +--------+             |          |
  |          |     recv ES |        | send ES     |          |
  |   send H |     ,-------|  open  |-------.     | recv H   |
  |          |    /        |        |        \    |          |
  |          v   v         +--------+         v   v          |
  |      +----------+          |           +----------+      |
  |      |   half   |          |           |   half   |      |
  |      |  closed  |          | send R /  |  closed  |      |
  |      | (remote) |          | recv R    | (local)  |      |
  |      +----------+          |           +----------+      |
  |           |                |                 |           |
  |           | send ES /      |       recv ES / |           |
  |           | send R /       v        send R / |           |
  |           | recv R     +--------+   recv R   |           |
  | send R /  `----------->|        |<-----------'  send R / |
  | recv R                 | closed |               recv R   |
  `----------------------->|        |<----------------------'
                           +--------+

  send:   endpoint sends this frame
  recv:   endpoint receives this frame

  H:  HEADERS frame (with implied CONTINUATIONs)
  PP: PUSH_PROMISE frame (with implied CONTINUATIONs)
  ES: END_STREAM flag
  R:  RST_STREAM frame
```

CREATE-NEW-LOCAL-STREAM can be used to create open stream.

FIND-HTTP-STREAM-BY-ID possibly creates new streams in various states based on
received frame. Note that IDLE and CLOSED symbols can stand for actual streams.

MAYBE-END-STREAM "
  (create-new-local-stream function)
  (find-http-stream-by-id function)
  (maybe-end-stream function)
  (close-http2-stream function))

(defgeneric is-our-stream-id (connection id))

;;;; Streams lifecycle
;;;; Create
(defun create-new-local-stream (connection &optional pars)
  "Create new local stream of default class on CONNECTION. Additional PARS are
passed to the make-instance"
  (let ((stream (apply #'make-instance (get-stream-class connection)
                       :stream-id (get-id-to-use connection)
                       :peer-window-size (get-initial-peer-window-size connection)
                       :window-size (get-initial-window-size connection)
                       :connection connection
                       :state 'open
                       pars)))
    (incf (get-id-to-use connection) 2)
    (push stream (get-streams connection))
    stream))

(defun peer-opens-http-stream-really-open (connection stream-id state)
  (unless (> stream-id (get-last-id-seen connection))
    (connection-error 'new-stream-id-too-low connection
                      :stream-id stream-id
                      :max-seen-so-far (get-last-id-seen connection)))
  ;; todo: count and check open streams
  (setf (get-last-id-seen connection) stream-id)
  (push (make-instance (get-stream-class connection)
                       :stream-id stream-id
                       :state state
                       :window-size (get-initial-window-size connection)
                       :peer-window-size (get-initial-peer-window-size connection)
                       :connection connection)
        (get-streams connection))
  (car (get-streams connection)))

;;;; change state
(defun change-state-on-write-end (http-stream)
  "Change state of the stream when STREAM-END is sent."
  (with-slots (state) http-stream
    (ecase state
      (open (setf state 'half-closed/local))
      (half-closed/remote (close-http2-stream http-stream)))))

(defun find-http-stream-by-id (connection id frame-type)
  "Find HTTP stream in the connection.

Returns either HTTP2-STREAM object (existing or new), CONNECTION or one of IDLE
and CLOSED for yet or already nonexistent streams.

Also do some checks on the stream id based on the frame type."
  (declare (optimize speed (safety 1) (debug 0))
           (type frame-type frame-type))
  (with-slots (streams last-id-seen) connection
    (declare (type stream-id id last-id-seen))
    (let ((new-stream-state (frame-type-new-stream-state frame-type))
          (our-id (is-our-stream-id connection id)))
      (cond
        ((zerop id)
         (if (frame-type-connection-ok frame-type)
             connection
             (connection-error 'frame-type-needs-stream connection :frame-type frame-type)))
        ((and (not our-id) (> id last-id-seen) new-stream-state)
         (peer-opens-http-stream-really-open connection id new-stream-state))
        ((and our-id (>= id (the stream-id (get-id-to-use connection))))
         (connection-error 'our-id-created-by-peer connection))
        ((and (not our-id) (> id last-id-seen))
         (connection-error 'bad-stream-state connection
                           :actual 'idle
                           :code +protocol-error+
                           :allowed '(not idle)
                           :stream-id id))
        ((frame-type-old-stream-ok frame-type)
         (check-stream-state-ok connection
                                (find-just-stream-by-id streams id)
                                (frame-type-old-stream-ok frame-type)
                                (frame-type-bad-state-error frame-type)))
        (t
         (connection-error 'frame-type-needs-connection connection
                           :frame-type frame-type))))))

(defun maybe-end-stream (flags stream)
  "Close the STREAM if FLAGS indicates so. It changes state to either of"
  (when (get-flag flags :end-stream)
    ;; 20240822 TODO: give specific error instead of ecase
    (ecase (get-state stream)
      (half-closed/local (close-http2-stream stream))
      (open (setf (get-state stream) 'half-closed/remote)))
    (peer-ends-http-stream stream)))


;;;; Introspection
(defgeneric get-state (state)
  (:method ((state (eql :closed))) 'closed)
  (:documentation
   "State of a HTTP stream. The parameter is either a HTTP2-STREAM object (that
keeps state in an appropriate slot) or placeholder values CLOSED or IDLE"))


(defun find-just-stream-by-id (streams id)
  "Find STREAM by ID in STREAMS, or :closed

The list of streams should already be sorted from high number to low number, so we could
stop as soon as we can see lower value. However, we assume the list needed to be searched is
pretty short so we do not care."
  (or (find id streams :test #'= :key #'get-stream-id) :closed))

(defun check-stream-state-ok (connection http-stream ok-states bad-state-error)
  "Throw BAD-STATE-ERROR when the stream state is not appropriate for the frame type.

Return the HTTP-STREAM."
  (unless (member (get-state http-stream) ok-states)
    (connection-error 'bad-stream-state  connection
                      :code bad-state-error
                      :actual (get-state http-stream)
                      :allowed ok-states))
  http-stream)
