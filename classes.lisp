(in-package http2)


;;;; Callbacks from frame reading functions

#|

The lifecycle of a stream is shown in Figure 2.

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

|#

(defun count-open-streams (connection)
  (count '(open half-closed/local half-closed/remote) (get-streams connection) :key #'get-state :test 'member))

(defgeneric peer-opens-http-stream (connection stream-id frame-type)
  (:documentation
   "Unknown stream ID was sent by the other side - i.e., from headers frame. Should
return an object representing new stream.")
  (:method (connection stream-id (frame-type (eql #.+headers-frame+)))
    (unless (> stream-id (get-last-id-seen connection))
      (http2-error connection +protocol-error+
                   "The identifier of a newly established stream MUST be
                   numerically greater than all streams that the initiating
                   endpoint has opened or reserved.  This governs streams that
                   are opened using a HEADERS frame and streams that are
                   reserved using PUSH_PROMISE.  An endpoint that receives an
                   unexpected stream identifier MUST respond with a connection
                   error (Section 5.4.1) of type PROTOCOL_ERROR."))
    ;; todo: count and check open streams
    (make-instance (get-stream-class connection) :stream-id stream-id :state 'open)))

(defgeneric peer-ends-http-stream (stream))

(defgeneric peer-sends-push-promise (continuation stream)
  ;; this is not actually called (yet) and may need more parameters
  (:method (continuation stream) (error "Push promises not supported.")))

(defgeneric peer-resets-stream (stream error-code)
  (:method (stream error-code)
    (setf (get-state stream) 'closed))
  (:documentation
   "The RST_STREAM frame fully terminates the referenced stream and
   causes it to enter the \"closed\" state.  After receiving a RST_STREAM
   on a stream, the receiver MUST NOT send additional frames for that
   stream, with the exception of PRIORITY.  However, after sending the
   RST_STREAM, the sending endpoint MUST be prepared to receive and
   process additional frames sent on the stream that might have been
   sent by the peer prior to the arrival of the RST_STREAM."))

#+nil (defgeneric peer-pushes-promise)

;;;; Other callbacks
(defgeneric apply-data-frame (connection stream payload)
  (:documentation "Data frame is received."))

(defgeneric apply-stream-priority (stream exclusive weight stream-dependency)
  (:method  (stream exclusive weight stream-dependency)
    (setf (get-weight stream) weight
          (get-depends-on stream)
          `(,(if exclusive :exclusive :non-exclusive) ,stream-dependency))))

(defgeneric apply-window-size-increment (object increment)
  (:method (object increment)
    (when (zerop increment)
      ;; fixme: why?
      (http2-error object +protocol-error+ ""))
    (setf (get-window-size object) increment)))

(defmethod update-dynamic-table-size (connection new-size)
  (adjust-array (get-dynamic-table connection) new-size))

(defmethod set-peer-setting (connection name value)
  (warn "Peer settings not used - ~a ~a." name value))

(defmethod set-peer-setting (connection (name (eql :header-table-size)) value)
  ;; do something
  )

(defmethod set-peer-setting (connection (name (eql :initial-window-size)) value)
  ;; do something
  )

(defmethod set-peer-setting (connection (name (eql :max-frame-size)) value)
  ;; do something
  )

(defmethod set-peer-setting (connection (name (eql :max-header-list-size)) value)
  ;; do something
  )

(defmethod set-peer-setting (connection (name (eql :max-concurrent-streams)) value)
  ;; do something
  )

(defmethod peer-expects-settings-ack (connection)
;  (write-ack-setting-frame (get-network-stream connection))
;  (write-settings-frame connection connection nil :ack t)
;  (force-output (get-network-stream connection))
  )

(defmethod peer-acks-settings (connection)
  ())


;;;; Classes
(defclass stream-or-connection ()
  ((window-size  :accessor get-window-size  :initarg :window-size)))


(defclass client-http2-connection (http2-connection)
  ()
  (:default-initargs :id-to-use 1))

(defmethod peer-opens-http-stream ((connection client-http2-connection) stream-id frame-type)
  (if (evenp stream-id)
      ;; no formal reaction observed
      (warn "Strems initiated by the server MUST use even-numbered stream identifiers.")))

(defclass server-http2-connection (http2-connection)
  ()
  (:default-initargs :id-to-use 2))

(defmethod peer-opens-http-stream ((connection client-http2-connection) stream-id frame-type)
  (if (oddp stream-id)
      ;; no formal reaction observed
      (warn "Streams initiated by a client MUST use odd-numbered stream identifiers")))

(defclass http2-connection (stream-or-connection)
  ((network-stream      :accessor get-network-stream       :initarg :network-stream)
   (streams             :accessor get-streams              :initarg :streams
                        :documentation "Sequence of HTTP2 streams")
   (acked-settings      :accessor get-acked-settings       :initarg :acked-settings)
   (dynamic-table       :accessor get-dynamic-table        :initarg :dynamic-table)
   (last-id-seen        :accessor get-last-id-seen         :initarg :last-id-seen)
   (id-to-use           :accessor get-id-to-use            :initarg :id-to-use
                        :type          (unsigned-byte 31)
                        :documentation
                        "Streams are identified with an unsigned 31-bit
                        integer.  Streams initiated by a client MUST
                        use odd-numbered stream identifiers; those
                        initiated by the server MUST use
                        even-numbered stream identifiers.  A stream
                        identifier of zero (0x0) is used for
                        connection control messages; the stream
                        identifier of zero cannot be used to
                        establish a new stream.")
   (expect-continuation :accessor get-expect-continuation
                        :initarg :expect-continuation
                        :type          (or null (unsigned-byte 31))
                        :initform       nil
                        :documentation
                        "A HEADERS frame without the END_HEADERS flag set MUST be
                        followed by a CONTINUATION frame for the same
                        stream. This slot is set to the id of the expected
                        stream in such case, and is nil otherwise.")
   (stream-class        :accessor get-stream-class         :initarg :stream-class)
   (enable-push         :accessor get-enable-push          :initarg :enable-push))
  (:default-initargs :id-to-use 1
                     :enable-push 0 ; disable by default. Use a mixin to enable.
                     :last-id-seen 0
                     :streams nil
                     :acked-settings nil
                     :window-size 0
                     :dynamic-table (make-array 0 :fill-pointer 0 :adjustable t)
                     :stream-class 'http2-stream)
  (:documentation
   "A simple connection: promise push not allowed, empty dynamic header table,
   reasonable behaviour"))

(defun dynamic-table-entry-size (name value)
  "The size of an entry is the sum of its name's length in octets (as
   defined in Section 5.2), its value's length in octets, and 32.

   The size of an entry is calculated using the length of its name and
   value without any Huffman encoding applied."
  (+ 32 (length name) (length value)))

(defmethod get-settings (connection)
  `((:max-frame-size . ,*max-frame-size*)
    (:enable-push . ,(get-enable-push connection))
    (:header-table-size . ,0)))

(defmethod get-stream-id ((conn http2-connection)) 0)

(defclass http2-stream (stream-or-connection)
  ((stream-id  :accessor get-stream-id  :initarg :stream-id
               :type (unsigned-byte 31))
   (state      :accessor get-state      :initarg :state
               :type (member idle open closed
                             half-closed/local half-closed/remote
                             reserved/local reserved/remote))
   (data       :accessor get-data       :initarg :data)
   (weight     :accessor get-weight     :initarg :weight)
   (depends-on :accessor get-depends-on :initarg :depends-on))
  (:default-initargs :state 'idle :window-size 0
   ;;   All streams are initially assigned a non-exclusive dependency on
   ;;   stream 0x0.  Pushed streams (Section 8.2) initially depend on their
   ;;   associated stream.  In both cases, streams are assigned a default
   ;;   weight of 16.
                     :weight 16
                     :depends-on '(:non-exclusive 0)
   )
  (:documentation
   "An independent, bidirectional sequence of frames exchanged between
  the client and server within an HTTP/2 connection.  Streams have
  several important characteristics:

  A single HTTP/2 connection can contain multiple concurrently open
  streams, with either endpoint interleaving frames from multiple
  streams.

  Streams can be established and used unilaterally or shared by
  either the client or server.

  Streams can be closed by either endpoint.

  The order in which frames are sent on a stream is significant.
  Recipients process frames in the order they are received.  In
  particular, the order of HEADERS and DATA frames is semantically
  significant.

  Streams are identified by an integer.  Stream identifiers are
  assigned to streams by the endpoint initiating the stream."))

(defmethod print-object ((o http2-stream) out)
  (print-unreadable-object (o out :type t :identity nil)
    (format out "~a #~d" (get-state o)
            (get-stream-id o))))

(defun change-state (stream state-map other-case-error)
  "Change state from STREAM based on STATE-MAP. If original state not found in STATE-MAP and OTHER-CASE-ERROR is set, raise that (HTTP2) error."
  (let* ((old-state (get-state stream))
         (new-state (second (find old-state state-map :test #'member :key #'car))))
    (cond
      ((eq new-state old-state))
      ((eq new-state :keep))
      (new-state (setf (get-state stream) new-state))
      (other-case-error (http2-error stream other-case-error "Bad state transition")))
    new-state))

(defmethod apply-data-frame :before (connection (stream http2-stream) payload)
  "DATA frames are subject to flow control and can only be sent when a
   stream is in the \"open\" or \"half-closed (remote)\" state.  The entire
   DATA frame payload is included in flow control, including the Pad
   Length and Padding fields if present.  If a DATA frame is received
   whose stream is not in \"open\" or \"half-closed (local)\" state, the
   recipient MUST respond with a stream error (Section 5.4.2) of type
   STREAM_CLOSED."
  (change-state stream '(((open half-closed/local) :keep)) 'stream-closed))

(defmethod open-http-stream (connection (stream http2-stream))
  "This method handles state transitions involved in receiving HEADERS-FRAME.

idle: Sending or receiving a HEADERS frame causes the stream to become \"open\"."
  (change-state stream
                '(((idle) open)
                  ((open half-closed/local) :keep))
                'protocol-error))

(defmethod process-end-stream (connection (stream http2-stream))
  "Do relevant state changes when closing http stream (as part of received HEADERS or
PAYLOAD)."
  (change-state stream
                '(((open) half-closed/remote)
                  ((half-closed/local) closed))
                nil))

(defmethod add-header :around ((stream http2-stream) name value)
  "Decode compressed headers"
  (call-next-method stream
                    (etypecase name
                      ((or string symbol) name)
                      ((vector (unsigned-byte 8)) (decode-huffman name)))
                    (etypecase value
                      ((or null string integer) value) ; integer can be removed if we removed "200"
                      ((vector (unsigned-byte 8)) (decode-huffman value)))))

(defmethod add-header (stream name value)
  nil)

(defmethod do-ping (connection data)
  (write-ping-frame connection connection data :ack t))

(defmethod do-pong (connection data)
  ;; measure time?
  )

(define-condition peer-should-go-away (serious-condition)
  ((error-code     :accessor get-error-code     :initarg :error-code)
   (debug-data     :accessor get-debug-data     :initarg :debug-data)
   (last-stream-id :accessor get-last-stream-id :initarg :last-stream-id)))

(defmethod http2-error (connection error-code debug-code &rest args)
  (let ((formatted (apply #'format nil debug-code args)))
    (write-goaway-frame connection connection
                        0 ; fixme: last processed stream
                        error-code
                        (map 'vector 'char-code formatted)
                        nil)
    (error 'peer-should-go-away
           0 ; fixme: last processed stream
           error-code formatted
           nil)))

(define-condition go-away (serious-condition)
  ((error-code     :accessor get-error-code     :initarg :error-code)
   (debug-data     :accessor get-debug-data     :initarg :debug-data)
   (last-stream-id :accessor get-last-stream-id :initarg :last-stream-id)))

(defmethod do-goaway (connection error-code last-stream-id debug-data)
  (error 'go-away :last-stream-id last-stream-id
                  :error-code error-code
                  :debug-data debug-data))

(defclass log-headers-mixin ()
  ()
  (:documentation "Class that logs some activities and state changes."))

(defmethod add-header :after ((stream log-headers-mixin) name value)
  (format t "~&header: ~a = ~a~%" name value))

(defclass header-collecting-mixin ()
  ((headers :accessor get-headers :initarg :headers))
  (:default-initargs :headers nil))

(defmethod add-header ((stream header-collecting-mixin) name value)
  (push (cons name value) (get-headers stream)))

(defclass body-collecting-mixin ()
  ((body :accessor get-body :initarg :body))
  (:default-initargs :body ""))

(defmethod apply-data-frame (connection (stream body-collecting-mixin) data)
  (setf (get-body stream) (concatenate 'string (get-body stream)
                                       (map 'string 'code-char  data)))
  (write-window-update-frame connection connection (length data) 0)
  (write-window-update-frame connection stream (length data) 0)
  (force-output (get-network-stream connection)))

(defmethod handle-undefined-frame (type flags length)
  "Callback that is called when a frame of unknown type is received - see
extensions..")


;;;; Connection and stream for logging: tracks every callback in (reversed)
;;;; history.
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

(defmethod do-goaway :before ((connection logging-connection) error-code last-stream-id debug-data)
  (add-log connection `(:go-away :last-stream-id ,last-stream-id
                           :error-code ,error-code)))

(defmethod do-goaway :around ((connection logging-connection) error-code last-stream-id debug-data)
  (call-next-method))

(defmethod apply-window-size-increment :before ((object logging-object) increment)
  (add-log object `(:window-size-increment ,increment)))

