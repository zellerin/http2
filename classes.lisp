(in-package http2)

(defclass stream-or-continuation ()
  ((window-size  :accessor get-window-size  :initarg :window-size)))

(defclass http2-connection ()
  ((network-stream      :accessor get-network-stream       :initarg :network-stream)
   (streams             :accessor get-streams              :initarg :streams
                        :documentation "Sequence of HTTP2 streams")
   (settings            :accessor get-settings             :initarg :settings)
   (peer-settings       :accessor get-peer-settings        :initarg :peer-settings)
   (acked-settings      :accessor get-acked-settings       :initarg :acked-settings)
   (dynamic-table       :accessor get-dynamic-table        :initarg :dynamic-table)
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
                        "A HEADERS frame without the END_HEADERS flag
                        set MUST be followed by a CONTINUATION frame
                        for the same stream.

                        This slot is set to the id of the expected
                        stream in such case, and is nil otherwise.")
   (window-size         :accessor get-window-size          :initarg :window-size)
   (stream-class        :accessor get-stream-class         :initarg :stream-class))
  (:default-initargs :id-to-use 1 :settings `((:max-frame-size . ,*max-frame-size*))
                     :streams nil
                     :acked-settings nil
                     :window-size 0
                     :dynamic-table (make-array 0 :fill-pointer 0 :adjustable t)
                     :stream-class 'http2-stream))

(defmethod get-stream-id ((conn http2-connection)) 0)

(defclass http2-stream ()
  ((stream-id   :accessor get-stream-id   :initarg :stream-id
                :type (unsigned-byte 31))
   (state       :accessor get-state       :initarg :state
                :type (member idle open closed
                              half-closed/local half-closed/remote
                              reserved/local reserved/remote))
   (window-size :accessor get-window-size :initarg :window-size)
   (data        :accessor get-data        :initarg :data))
  (:default-initargs :state 'idle :window-size 0)
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
      (new-state (setf (get-state stream) new-state)
                 (format t "~&~a -> ~a~%" old-state new-state))
      (other-case-error (http2-error other-case-error)))
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

(defmethod do-ping (connection data)
  (logger "Ping ~s" data))

(defmethod do-pong (connection data)
  (logger "Pong ~s" data))
