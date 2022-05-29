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
   (window-size         :accessor get-window-size          :initarg :window-size))
  (:default-initargs :id-to-use 1 :settings `((:max-frame-size . ,*max-frame-size*))
                     :streams nil
                     :acked-settings nil
                     :window-size 0
                     :dynamic-table (make-array 0 :fill-pointer 0 :adjustable t)))

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
