(in-package http2)

;;;; Classes
#|
                                 stream-or-connection
                                /                   \
                     http2-stream                   http2-connection--------------\
                     /   /                         /       \                       \
        logging-stream  /     client-http2-connection   server-http2-connection  logging-connection
                       /                      \
     (stream mixins)  /                        \        (connection mixins)
               \     /                          \             /
              sample-client-stream     sample-client-connection
|#

(defclass stream-or-connection ()
  ((window-size  :accessor get-window-size  :initarg :window-size)))

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

(defclass client-http2-connection (http2-connection)
  ()
  (:default-initargs :id-to-use 1))

(defclass server-http2-connection (http2-connection)
  ()
  (:default-initargs :id-to-use 2))

(defclass http2-stream (stream-or-connection)
  ((stream-id        :accessor get-stream-id        :initarg :stream-id
                     :type (unsigned-byte 31))
   (state            :accessor get-state            :initarg :state
                     :type (member idle open closed
                                   half-closed/local half-closed/remote
                                   reserved/local reserved/remote))
   (data             :accessor get-data             :initarg :data)
   (weight           :accessor get-weight           :initarg :weight)
   (depends-on       :accessor get-depends-on       :initarg :depends-on)
   (seen-text-header :accessor get-seen-text-header :initarg :seen-text-header
                     :documentation
                     "Set if in the header block a non-pseudo header was already seen."))
  (:default-initargs :state 'idle :window-size 0
   ;;   All streams are initially assigned a non-exclusive dependency on
   ;;   stream 0x0.  Pushed streams (Section 8.2) initially depend on their
   ;;   associated stream.  In both cases, streams are assigned a default
   ;;   weight of 16.
                     :weight 16
                     :depends-on '(:non-exclusive 0)
                     :seen-text-header nil
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

(defclass client-stream (http2-stream)
  ((status :accessor get-status :initarg :status
           :documentation
           "HTTP status code field (see [RFC7231], Section 6)"))
  (:default-initargs :status nil))

(defclass server-stream (http2-stream)
  ((method    :accessor get-method    :initarg :method
              :documentation
              "The HTTP method ([RFC7231], Section 4)")
   (scheme    :accessor get-scheme    :initarg :scheme
              :documentation
              "Scheme portion of the target URI ([RFC3986], Section 3.1).

               Not restricted to \"http\" and \"https\" schemed URIs.
               A proxy or gateway can translate requests for non-HTTP schemes,
               enabling the use of HTTP to interact with non-HTTP services")
   (authority :accessor get-authority :initarg :authority
              :documentation
              "The authority portion of the target URI ([RFC3986], Section 3.2)")
   (path      :accessor get-path      :initarg :path
              :documentation
              "The path and query parts of the target URI"))
  (:default-initargs :method nil :scheme nil :authority nil :path nil))

(defclass log-headers-mixin ()
  ()
  (:documentation "Class that logs some activities and state changes."))

(defclass header-collecting-mixin ()
  ((headers :accessor get-headers :initarg :headers))
  (:default-initargs :headers nil)
  (:documentation
   "Mixin to be used to collect all observed headers to a slot."))

(defclass body-collecting-mixin ()
  ((body :accessor get-body :initarg :body))
  (:default-initargs :body "")
  (:documentation
   "Mixin to collect all payload parts to one string."))
;;;; Q: can one UTF-8 character be split into two frames?

(defclass timeshift-pinging-connection ()
  ())

;;;; Connection and stream for logging: tracks every callback in (reversed)
;;;; history.
(defclass logging-object ()
  ())

(defclass history-keeping-object (logging-object)
  ((reversed-history :accessor get-reversed-history :initarg :reversed-history))
  (:default-initargs :reversed-history nil))

(defclass history-printing-object (logging-object)
  ())

(defclass logging-connection (http2-connection history-keeping-object)
  ())

(defclass logging-stream (http2-stream history-keeping-object)
  ())


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

(defmethod (setf get-state) :around (value (stream logging-object))
  (let ((before (get-state stream)))
    (add-log stream `(:state-change ,before -> ,value))
    (call-next-method))
  (call-next-method))

(defgeneric send-headers (connection stream headers &key end-stream)
  (:documentation
   "Send headers to the connection and stream. Stream is either an existing
instance of a stream, or a symbol :new. Stream is returned.

In future should ensure splitting too long headers to continuations, but does
not now.")
  (:method (connection (stream http2-stream) headers &key end-stream)
    (write-headers-frame connection stream headers
                         :end-headers t :end-stream end-stream)
    stream)

  (:method :before (connection (stream logging-object) headers &key end-stream)
    (add-log stream `(:sending-headers ,headers :end-stream ,end-stream)))
  (:method (connection (stream (eql :new)) headers &key end-stream)
    (let ((new-stream (create-new-local-stream connection)))
      (setf (get-state new-stream) 'open)
      (send-headers connection new-stream headers :end-stream end-stream))))

(defgeneric peer-opens-http-stream (connection stream-id frame-type)
  (:documentation
   "Unknown stream ID was sent by the other side - i.e., from headers frame. Should
 return an object representing new stream.")
  (:method :before ((connection logging-object) stream-id frame-type)
    (add-log connection `(:new-stream-received :id ,stream-id :type
                                               ,(frame-type-name (aref *frame-types* frame-type)))))
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
    (push (make-instance (get-stream-class connection) :stream-id stream-id :state 'open)
          (get-streams connection))
    (car (get-streams connection)))

  (:method (connection stream-id (frame-type (eql #.+priority-frame+)))
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
      (push (make-instance (get-stream-class connection) :stream-id stream-id :state 'idle)
            (get-streams connection))
      (car (get-streams connection)))
  (:method :after ((connection logging-object) stream-id frame-type)
    (add-log connection `(:new-stream-requested ,stream-id ,frame-type)))

  (:method :before ((connection client-http2-connection) stream-id frame-type)
    (if (oddp stream-id)
        ;; Q: does RFC document what to do?
        (warn "Strems initiated by the server MUST use even-numbered stream identifiers.")))

  (:method :before ((connection server-http2-connection) stream-id frame-type)
    (when (evenp stream-id)
        ;; Q: does RFC document what to do?
      (warn "Streams initiated by a client MUST use odd-numbered stream identifiers")))

  (:method :around ((connection logging-object) stream-id frame-type)
    (let ((stream (call-next-method)))
      (add-log stream `(:opened :id ,stream-id
                                :frame-type ,(frame-type-name (aref *frame-types* frame-type))))
      stream)))

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

(defgeneric send-stream-error (connection stream error-code note)
  (:method (connection (stream http2-stream) error-code note)
    (write-rst-stream-frame connection stream error-code)
    (warn "Stream error: rst frame sent locally - ~s" note)
    (setf (get-state stream) 'closed)))

;;;; Other callbacks
(defgeneric apply-data-frame (connection stream payload)
  (:documentation "Data frame is received.")

  (:method (connection stream payload)
    (warn "No payload action defined."))

  (:method (connection (stream body-collecting-mixin) data)
    (setf (get-body stream)
          (concatenate 'string (get-body stream)
                       (map 'string 'code-char  data)))
    (write-window-update-frame connection connection (length data) 0)
    (write-window-update-frame connection stream (length data) 0)
    (force-output (get-network-stream connection)))

  (:method :before ((connection logging-object) (stream logging-object)
                    payload)
    (add-log stream `(:payload ,payload)))

#+nil  (:method :after (connection (stream http2-stream) payload)
    "DATA frames are subject to flow control and can only be sent when a
   stream is in the \"open\" or \"half-closed (remote)\" state.  The entire
   DATA frame payload is included in flow control, including the Pad
   Length and Padding fields if present.  If a DATA frame is received
   whose stream is not in \"open\" or \"half-closed (local)\" state, the
   recipient MUST respond with a stream error (Section 5.4.2) of type
   STREAM_CLOSED."
    (change-state stream '(((open half-closed/local) :keep)) 'stream-closed)))

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
  (setf (fill-pointer (get-dynamic-table connection)) new-size)
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
  (write-settings-frame connection connection nil :ack t)
  (force-output (get-network-stream connection)))

(defmethod peer-acks-settings (connection)
  ())

(defmethod peer-acks-settings ((connection logging-object))
  (add-log connection '(:settings-acked)))


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

(defmethod print-object ((o http2-stream) out)
  (print-unreadable-object (o out :type t :identity nil)
    (format out "~a #~d" (get-state o)
            (get-stream-id o))))

(defun change-state (stream state-map other-case-error)
  "Change state from STREAM based on STATE-MAP. If original state not found in STATE-MAP and OTHER-CASE-ERROR is set, raise that (HTTP2) error."
  (cerror "I am not used" "OK")
  (let* ((old-state (get-state stream))
         (new-state (second (find old-state state-map :test #'member :key #'car))))
    (cond
      ((eq new-state old-state))
      ((eq new-state :keep))
      (new-state (setf (get-state stream) new-state))
      (other-case-error (http2-error stream other-case-error "Bad state transition")))
    new-state))

(defmethod open-http-stream (connection (stream http2-stream))
  "This method handles state transitions involved in receiving HEADERS-FRAME.

idle: Sending or receiving a HEADERS frame causes the stream to become \"open\"."
  (change-state stream
                '(((idle) open)
                  ((open half-closed/local) :keep))
                'protocol-error))

(defgeneric peer-ends-http-stream (connection stream)
  (:documentation
   "Do relevant state changes when closing http stream (as part of received HEADERS or
PAYLOAD).")
  (:method :after (connection (stream http2-stream))
    (setf (get-state stream)
          (ecase (get-state stream)
            ((open) 'half-closed/remote)
            ((half-closed/local) 'closed))))
  (:method :after (connection (stream logging-object))
    (add-log stream '(:closed-remotely))))

(defmacro check-place-empty-and-set-it (new-value accessor)
  "All HTTP/2 requests MUST include exactly one valid value for the
   :method, :scheme, and :path pseudo-header fields, unless it is
   a CONNECT request (Section 8.3).  An HTTP request that omits
   mandatory pseudo-header fields is malformed (Section 8.1.2.6)."
  ;; fixme: use place expanders
  `(if (,accessor stream)
       (send-stream-error connection stream  +protocol-error+
                          "Duplicated header")
       (setf (,accessor stream) ,new-value)))

(defgeneric add-header (connection stream name value)
  (:method :around (connection (stream http2-stream) name value)
    "Decode compressed headers"
    (let ((decoded-name
            (etypecase name
              ((or string symbol) name)
              ((vector (unsigned-byte 8)) (decode-huffman name)))))
      (when (and (stringp name) (some #'upper-case-p name))
        (send-stream-error connection stream  +protocol-error+
                           "A request or response containing uppercase header field names MUST be treated as malformed. (...) Malformed requests or responses that are detected MUST be
   treated as a stream error of type PROTOCOL_ERROR. "))
      (call-next-method connection stream
                        decoded-name
                        (etypecase value
                          (string value) ; integer can be removed if we removed "200"
                          ((vector (unsigned-byte 8)) (decode-huffman value))))))

  (:method (connection stream name value)
    (warn "You should overwrite default method for adding new header."))

  (:method (connection (stream server-stream) (name symbol) value)
    (when (get-seen-text-header stream)
      (send-stream-error connection stream  +protocol-error+
                         "Pseudo header follows text header."))
    (case name
      (:method
       (check-place-empty-and-set-it value get-method))
      (:scheme
       (check-place-empty-and-set-it value get-scheme))
      (:authority
       (check-place-empty-and-set-it value get-authority))
      (:path
       (check-place-empty-and-set-it value get-path))
      (t
       (send-stream-error connection stream  +protocol-error+
                          "Incorrect pseudo header on response"))))

  (:method (connection (stream client-stream) (name symbol) value)
    (when (get-seen-text-header stream)
      (send-stream-error connection stream  +protocol-error+
                         "Pseudo header follows text header."))
    (case name
      (:status
          (setf (get-status stream) value))
      (t
       (send-stream-error connection stream  +protocol-error+
                          "Incorrect pseudo header on request"))))

  (:method :after (connection (stream log-headers-mixin) name value)
    (format t "~&header: ~a = ~a~%" name value))

  (:method (connection (stream header-collecting-mixin) name value)
    (push (cons name value) (get-headers stream)))

  (:method :before (connection (stream logging-object) name value)
    (add-log stream `(:header ,name ,value))))



(defgeneric send-ping (connection &optional payload)
  (:documentation
   "Send a ping request.")
  (:method (connection &optional (payload 0))
    (declare ((unsigned-byte 64) payload))
    (write-ping-frame connection connection payload))
  (:method ((connection timeshift-pinging-connection) &optional payload)
    (declare (ignore payload))
    (call-next-method connection (mod (get-internal-real-time) (expt 2 64)))))

(defgeneric process-end-headers (connection stream)
  (:method  :before (connection (stream logging-object))
    (add-log stream '(:end-headers)))

  (:method (connection stream)
    ;; anything sane to do?
    ))

(defmethod do-ping (connection data)
  (write-ping-frame connection connection data :ack t))

(defgeneric do-pong (connection data)
  (:method (connection data)
    (warn "The connection ~a did not expect ping response" connection))
  (:method ((connection timeshift-pinging-connection) data)
    (format t "Ping time: ~5fs~%" (/ (- (get-internal-real-time) data) 1.0 internal-time-units-per-second))))

(define-condition peer-should-go-away (serious-condition)
  ((error-code     :accessor get-error-code     :initarg :error-code)
   (debug-data     :accessor get-debug-data     :initarg :debug-data)
   (last-stream-id :accessor get-last-stream-id :initarg :last-stream-id)))

(defmethod http2-error (connection error-code debug-code &rest args)
  (let ((formatted (apply #'format nil debug-code args)))
    (write-goaway-frame connection connection
                        0 ; fixme: last processed stream
                        error-code
                        (map 'vector 'char-code formatted) )
    #+nil(error 'peer-should-go-away
           :last-stream-id 0 ; fixme: last processed stream
           :error-code error-code
           :debug-data formatted)
    nil))

(define-condition go-away (serious-condition)
  ((error-code     :accessor get-error-code     :initarg :error-code)
   (debug-data     :accessor get-debug-data     :initarg :debug-data)
   (last-stream-id :accessor get-last-stream-id :initarg :last-stream-id)))

(defmethod do-goaway (connection error-code last-stream-id debug-data)
  (unless (eq error-code '+no-error+)
    (error 'go-away :last-stream-id last-stream-id
                    :error-code error-code
                    :debug-data debug-data)))

(defmethod handle-undefined-frame (type flags length)
  "Callback that is called when a frame of unknown type is received - see
extensions..")


(defvar *do-print-log* nil
  "Set to true value to log to stderr.")

(defgeneric add-log (object log-pars)
  (:method ((object history-keeping-object) log-pars)
    (push log-pars (get-reversed-history object)))
  (:method ((object history-printing-object) log-pars)
    (when *do-print-log*
      (let ((*print-length* 10))
        (format t "~&~s: ~{~s~^ ~}~%" object log-pars)))))

(defun get-history (object)
  (reverse (get-reversed-history object)))

(defmethod apply-stream-priority ((stream logging-object) exclusive weight stream-dependency)
  (add-log stream `(:new-prio :exclusive ,exclusive :weight ,weight :dependency ,stream-dependency)))

(defmethod initialize-instance :after ((connection server-http2-connection) &key &allow-other-keys)
  (let ((preface-buffer (make-array (length +client-preface-start+))))
    (read-sequence preface-buffer (get-network-stream connection))
    (unless (equalp preface-buffer +client-preface-start+)
      (warn "Client preface mismatch: got ~a" preface-buffer)))
  (write-settings-frame connection connection (get-settings connection))
  (force-output (get-network-stream connection)))

;; 3.5.  HTTP/2 Connection Preface
(defvar +client-preface-start+
  #.(vector-from-hex-text "505249202a20485454502f322e300d0a0d0a534d0d0a0d0a")
  "The client connection preface starts with a sequence of 24 octets, which in hex notation is this. That is, the connection preface starts with the string
 \"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n\").")

(defmethod initialize-instance :after ((connection client-http2-connection) &key &allow-other-keys)
  "In HTTP/2, each endpoint is required to send a connection preface as a
   final confirmation of the protocol in use and to establish the
   initial settings for the HTTP/2 connection.  The client and server
   each send a different connection preface.

   The client connection preface starts with a sequence of 24 octets.  This
   sequence MUST be followed by a SETTINGS frame (Section 6.5), which MAY be
   empty."

  (write-sequence +client-preface-start+ (get-network-stream connection))
  (write-settings-frame connection connection (get-settings connection))
  (force-output (get-network-stream connection)))

(defmethod peer-resets-stream ((stream logging-object) error-code)
  (add-log stream `(:closed :error ,(get-error-name error-code))))

(defmethod set-peer-setting :before ((connection logging-object) name value)
  (add-log connection `(:setting ,name ,value)))

(defmethod peer-expects-settings-ack :before ((connection logging-object))
  (add-log connection '(:settings-ack-needed)))

(defmethod do-ping :before ((connection logging-object) data)
  (add-log connection `(:ping ,data)))

(defmethod do-pong :before ((connection logging-object) data)
  (add-log connection `(:pong ,data)))

(defmethod do-goaway :before ((connection logging-object) error-code last-stream-id debug-data)
  (add-log connection `(:go-away :last-stream-id ,last-stream-id
                           :error-code ,error-code)))

(defmethod do-goaway :around ((connection logging-object) error-code last-stream-id debug-data)
  (call-next-method))

(defmethod apply-window-size-increment :before ((object logging-object) increment)
  (add-log object `(:window-size-increment ,increment)))

