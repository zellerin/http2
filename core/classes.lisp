;;;; Copyright 2022 by Tomáš Zellerin

(in-package :http2)

;;;; Classes
#|
                     http2-stream                   http2-connection--------------\
                     /   /                         /       \                       \
        logging-stream  /     client-http2-connection   server-http2-connection  logging-connection
                       /                      \
     (stream mixins)  /                        \        (connection mixins)
               \     /                          \             /
             vanilla-client-stream     vanilla-client-connection
|#

(defclass flow-control-mixin ()
  ((window-size      :accessor get-window-size      :initarg :window-size)
   (peer-window-size :accessor get-peer-window-size :initarg :peer-window-size))
  (:documentation
   "The flow control parameters are used both for streams and connections."))

(defclass http2-connection (flow-control-mixin)
  ((network-stream           :accessor get-network-stream           :initarg :network-stream)
   (streams                  :accessor get-streams                  :initarg :streams
                             :documentation "Sequence of HTTP2 streams")
   (acked-settings           :accessor get-acked-settings           :initarg :acked-settings)
   (compression-context      :accessor get-compression-context      :initarg :compression-context)
   (decompression-context    :accessor get-decompression-context    :initarg :decompression-context)
   (last-id-seen             :accessor get-last-id-seen             :initarg :last-id-seen)
   (id-to-use                :accessor get-id-to-use                :initarg :id-to-use
                             :type          (unsigned-byte 31)
                             :documentation
                             "Streams are identified with an unsigned 31-bit  integer.")
   (stream-class             :accessor get-stream-class             :initarg :stream-class
                             :documentation "Class for new streams")
   (initial-window-size      :accessor get-initial-window-size      :initarg :initial-window-size)
   (initial-peer-window-size :accessor get-initial-peer-window-size :initarg :initial-peer-window-size)
   (max-frame-size           :accessor get-max-frame-size           :initarg :max-frame-size)
   (max-peer-frame-size      :accessor get-max-peer-frame-size      :initarg :max-peer-frame-size)
   (stream-id                :accessor get-stream-id                :initarg :stream-id
                             :initform 0
                             :allocation :class))
  (:default-initargs :id-to-use 1
                     :last-id-seen 0
                     :streams nil
                     :acked-settings nil
                     :window-size 0
                     :compression-context (make-instance 'hpack-context)
                     :decompression-context (make-instance 'hpack-context)
                     :stream-class 'http2-stream
                     :initial-peer-window-size 65535
                     :initial-window-size 65535
                     :max-frame-size 16384
                     :max-peer-frame-size 16384
                     :peer-window-size 65535)

  (:documentation
   "A simple connection: promise push not allowed, otherwise reasonable behaviour"))

(defclass client-http2-connection (http2-connection)
  ()
  (:default-initargs :id-to-use 1)
  (:documentation
   "Client connections have odd-numbered streams."))

(defclass server-http2-connection (http2-connection)
  ((peer-accepts-push :accessor get-peer-accepts-push :initarg :peer-accepts-push))
  (:default-initargs :id-to-use 2
   :peer-accepts-push t))

(defclass http2-stream (flow-control-mixin)
  ((connection       :accessor get-connection       :initarg :connection)
   (network-stream   :accessor get-network-stream   :initarg :network-stream)
   (stream-id        :accessor get-stream-id        :initarg :stream-id
                     :type           (unsigned-byte 31))
   (state            :accessor get-state            :initarg :state
                     :type           (member idle open closed
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
                     :seen-text-header nil)
  (:documentation
   "Representation of HTTP/2 stream. See RFC7540."))

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

(defmethod print-object ((stream server-stream) out)
  (print-unreadable-object (stream out :type t)
    (format out "#~d ~s ~s" (get-stream-id stream) (get-path stream) (get-state stream))))

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

(defgeneric send-headers (stream-or-connection
                          headers &key end-stream end-headers
                                    padded priority &allow-other-keys)
  (:documentation
   "Send headers to the connection or stream. Stream is either an existing instance
of a stream, or a connection; in this case a new stream is created on it. In both
cases, the stream is returned.")

  (:method ((stream http2-stream) headers &key end-stream (end-headers t)
                                                      padded priority)
    (with-slots (connection) stream
      (when (get-updates-needed (get-compression-context connection))
        (warn "FIXME: we should send dynamical update."))
      (write-headers-frame stream headers
                           :padded padded
                           :priority priority
                           :end-stream end-stream
                           :end-headers end-headers))
    stream)

  (:method :before ((stream logging-object) headers &rest raw-stream-args)
    (add-log stream `(:sending-headers ,headers ,raw-stream-args)))

  (:method ((connection http2-connection) headers &rest stream-args
            &key end-stream (end-headers t)
              padding priority)
    (send-headers (create-new-local-stream connection `(:state open ,@stream-args))
                  headers
                  :padding padding
                  :priority priority
                  :end-stream end-stream
                  :end-headers end-headers)))

(defun peer-opens-http-stream-really-open (connection stream-id state)
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
    (push (make-instance (get-stream-class connection)
                         :stream-id stream-id
                         :state state
                         :window-size (get-initial-window-size connection)
                         :peer-window-size (get-initial-peer-window-size connection)
                         :connection connection
                         :network-stream (get-network-stream connection))
          (get-streams connection))
  (car (get-streams connection)))

(defgeneric peer-opens-http-stream (connection stream-id frame-type)
  (:documentation
   "Unknown stream ID was sent by the other side - i.e., from headers frame. Should
 return an object representing new stream.")
  (:method :before ((connection logging-object) stream-id frame-type)
    (add-log connection `(:new-stream-received :id ,stream-id :type
                                               ,(frame-type-name (aref *frame-types* frame-type)))))

  (:method (connection stream-id (frame-type (eql #.+headers-frame+)))
    (peer-opens-http-stream-really-open connection stream-id 'open))

  (:method (connection stream-id (frame-type (eql #.+priority-frame+)))
    (peer-opens-http-stream-really-open connection stream-id 'idle))

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

(defgeneric peer-sends-push-promise (stream)
  ;; this is not actually called (yet) and may need more parameters
  (:method (stream) (error "Push promises not supported.")))

(defgeneric peer-resets-stream (stream error-code)
  (:method (stream error-code)
    (setf (get-state stream) 'closed)
    (unless (eq error-code '+cancel+)
      (error 'http-stream-error :stream stream
                                :error-code error-code
                                :debug-data nil)))
  (:method :before ((stream logging-object) error-code)
    (add-log stream  `(:closed :error ,(get-error-name error-code))))

  (:documentation
   "The RST_STREAM frame fully terminates the referenced stream and
   causes it to enter the \"closed\" state.  After receiving a RST_STREAM
   on a stream, the receiver MUST NOT send additional frames for that
   stream, with the exception of PRIORITY.  However, after sending the
   RST_STREAM, the sending endpoint MUST be prepared to receive and
   process additional frames sent on the stream that might have been
   sent by the peer prior to the arrival of the RST_STREAM."))

#+nil (defgeneric peer-pushes-promise)

(defgeneric send-stream-error (stream error-code note)
  (:method ((stream http2-stream) error-code note)
    (with-slots (connection) stream
      (write-rst-stream-frame stream error-code)
      (force-output (get-network-stream connection))
      (warn "Stream error: rst frame sent locally - ~s" note)
      (setf (get-state stream) 'closed))))

;;;; Other callbacks
(defgeneric apply-data-frame (stream payload)
  (:documentation "Data frame is received.")

  (:method (stream payload)
    (warn "No payload action defined."))

  (:method ((stream body-collecting-mixin) data)
    (setf (get-body stream)
          (concatenate 'string (get-body stream)
                       (map 'string 'code-char  data)))
    (with-slots (connection) stream
      (write-window-update-frame connection (length data))
      (write-window-update-frame stream (length data))))

  (:method :before ((stream logging-object) payload)
    (add-log stream `(:payload ,payload))))


(defgeneric apply-stream-priority (stream exclusive weight stream-dependency)
  (:method  (stream exclusive weight stream-dependency)
    (setf (get-weight stream) weight
          (get-depends-on stream)
          `(,(if exclusive :exclusive :non-exclusive) ,stream-dependency)))

  (:method :before ((stream logging-object) exclusive weight stream-dependency)
    (add-log stream `(:new-prio :exclusive ,exclusive
                                :weight ,weight :dependency ,stream-dependency))))

(defgeneric apply-window-size-increment (object increment)

  (:method :before ((object logging-object) increment)
    (add-log object `(:window-size-increment ,increment)))

  (:method (object increment)
    (when (zerop increment)
      ;; fixme: why?
      (http2-error object +protocol-error+ ""))
    (incf (get-window-size object) increment)))

(defgeneric set-peer-setting (connection name value)
  (:documentation
   "Process received information about peers setting.

The setting relates to the CONNECTION. NAME is a keyword symbol (see
*SETTINGS-ALIST*, subject to possible change to 16bit number in future) and VALUE is
32bit number.")
  (:method (connection name value)
    "Fallback."
    (declare (type (unsigned-byte 32) value))
    (warn "Peer settings not used - ~a ~a." name value))

  (:method :before ((connection logging-object) name value)
    (declare (type (unsigned-byte 32) value))
    (add-log connection `(:setting ,name ,value)))

  (:method (connection (name (eql :header-table-size)) value)
    (declare (type (unsigned-byte 32) value))
    (let ((context (get-compression-context connection)))
      (when (> value (the (unsigned-byte 32) (get-dynamic-table-size context)))
        (update-dynamic-table-size context value)
        (push value (get-updates-needed context)))))

  (:method (connection (name (eql :initial-window-size)) value)
    (declare (type (unsigned-byte 32) value))
    (setf (get-initial-peer-window-size connection) value))

  (:method (connection (name (eql :max-frame-size)) value)
    (if (>= 16777215 value 16384)
        (setf (get-max-peer-frame-size connection) value)
        (http2-error +protocol-error+
                     "Frame size MUST be between the initial value 16384 and the maximum allowed frame
size (2^24-1 or 16,777,215 octets), inclusive, and is ~d" value)))

  (:method (connection (name (eql :max-header-list-size)) value)
    ;; do something
    )

  (:method ((connection client-http2-connection) (name (eql :enable-push)) value)
    (declare (type (unsigned-byte 32) value))
    (unless (= value 0)
      (http2-error connection +protocol-error+ "Server must have ENABLE-PUSH 0.")))

  (:method ((connection server-http2-connection) (name (eql :enable-push)) value)
    (declare (type (unsigned-byte 32) value))
    (if (> value 1)
        (http2-error connection  +protocol-error+ "Client must have ENABLE-PUSH 0 or 1.")
        (setf (get-peer-accepts-push connection) (plusp value))))

  (:method (connection (name (eql :max-concurrent-streams)) value)
    ;; do something
    ))

(defgeneric peer-expects-settings-ack (connection)
  (:method (connection)
    (write-settings-frame connection nil :ack t))

  (:method :before ((connection logging-object))
    (add-log connection '(:settings-ack-needed))))


(defgeneric peer-acks-settings (connection)
  (:method (connection))

  (:method ((connection logging-object))
    (add-log connection '(:settings-acked))))


(defun dynamic-table-entry-size (name value)
  "The size of an entry is the sum of its name's length in octets (as
   defined in Section 5.2), its value's length in octets, and 32.

   The size of an entry is calculated using the length of its name and
   value without any Huffman encoding applied."
  (+ 32 (length name) (length value)))

(defgeneric get-settings (connection)
  (:method-combination append)
  (:method append (connection)
    `((:max-frame-size . ,(get-max-peer-frame-size connection))
      (:header-table-size .
                          ,(get-dynamic-table-size (get-decompression-context connection)))))
  (:method append ((connection client-http2-connection))
    `((:enable-push . 0))))

(defmethod print-object ((o http2-stream) out)
  (print-unreadable-object (o out :type t :identity nil)
    (format out "~a #~d" (get-state o)
            (get-stream-id o))))

(defgeneric peer-ends-http-stream (stream)
  (:documentation
   "Do relevant state changes when closing http stream (as part of received HEADERS or
PAYLOAD).")
  (:method :after ((stream http2-stream))
    (setf (get-state stream)
          (ecase (get-state stream)
            ((open) 'half-closed/remote)
            ((half-closed/local) 'closed))))
  (:method :after ((stream logging-object))
    (add-log stream '(:closed-remotely))))

(defmacro check-place-empty-and-set-it (new-value accessor)
  "All HTTP/2 requests MUST include exactly one valid value for the
   :method, :scheme, and :path pseudo-header fields, unless it is
   a CONNECT request (Section 8.3).  An HTTP request that omits
   mandatory pseudo-header fields is malformed (Section 8.1.2.6)."
  ;; fixme: use place expanders
  `(if (,accessor stream)
       (send-stream-error stream  +protocol-error+
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
        (send-stream-error stream  +protocol-error+
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
      (send-stream-error stream  +protocol-error+
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
       (send-stream-error stream  +protocol-error+
                          "Incorrect pseudo header on response"))))

  (:method (connection (stream client-stream) (name symbol) value)
    (when (get-seen-text-header stream)
      (send-stream-error stream  +protocol-error+
                         "Pseudo header follows text header."))
    (case name
      (:status
          (setf (get-status stream) value))
      (t
       (send-stream-error stream  +protocol-error+
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
    (write-ping-frame connection payload))
  (:method ((connection timeshift-pinging-connection) &optional payload)
    (declare (ignore payload))
    (call-next-method connection (mod (get-internal-real-time) (expt 2 64)))))

(defgeneric process-end-headers (connection stream)
  (:method  :before (connection (stream logging-object))
    (add-log stream '(:end-headers)))

  (:method (connection stream))

  (:method (connection (stream client-stream))
    ;; Headers sanity check
    (unless (get-status stream)
      (send-stream-error stream  +protocol-error+
                         ":status pseudo-header field MUST be included in all responses"))
    ;; next header section may contain another :status
    (setf (get-seen-text-header stream) nil))

  (:method (connection (stream server-stream))
    ;; Headers sanity check
    (unless (or (equal (get-method stream) "CONNECT")
             (and (get-method stream) (get-scheme stream) (get-path stream)))
      (send-stream-error stream  +protocol-error+
                         "All HTTP/2 requests MUST include exactly one valid value for the
   :method, :scheme, and :path pseudo-header fields, unless it is
   a CONNECT request"))))

(defgeneric do-ping (connection data)
  (:method (connection data)
    (write-ping-frame connection data :ack t))
  (:method :before ((connection logging-object) data)
    (add-log connection `(:ping ,data))))

(defgeneric do-pong (connection data)
  (:method (connection data)
    (warn "The connection ~a did not expect ping response" connection))
  (:method ((connection timeshift-pinging-connection) data)
    (format t "Ping time: ~5fs~%" (/ (- (get-internal-real-time) data) 1.0 internal-time-units-per-second))))

(define-condition peer-should-go-away (serious-condition)
  ((error-code     :accessor get-error-code     :initarg :error-code)
   (debug-data     :accessor get-debug-data     :initarg :debug-data)
   (last-stream-id :accessor get-last-stream-id :initarg :last-stream-id)))

(define-condition http-stream-error (serious-condition)
  ((error-code :accessor get-error-code :initarg :error-code)
   (debug-data :accessor get-debug-data :initarg :debug-data)
   (stream     :accessor get-stream     :initarg :stream)))

(defmethod http2-error (connection error-code debug-code &rest args)
  (let ((formatted (apply #'format nil debug-code args)))
    (write-goaway-frame connection
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
    (when (or (eq *do-print-log* t)
              (member (car log-pars)  *do-print-log*))
      (let ((*print-length* 10)
            (*print-base* 16))
        (format t "~&~s: ~{~s~^ ~}~%" object log-pars)))))

(defun get-history (object)
  (reverse (get-reversed-history object)))


(defmethod initialize-instance :after ((connection server-http2-connection) &key &allow-other-keys)
  (let ((preface-buffer (make-array (length +client-preface-start+))))
    (read-sequence preface-buffer (get-network-stream connection))
    (unless (equalp preface-buffer +client-preface-start+)
      (warn "Client preface mismatch: got ~a" preface-buffer)))
  (write-settings-frame connection (get-settings connection)))

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
  (write-settings-frame connection (get-settings connection)))

(defmethod do-pong :before ((connection logging-object) data)
  (add-log connection `(:pong ,data)))

(defmethod do-goaway :before ((connection logging-object) error-code last-stream-id debug-data)
  (add-log connection `(:go-away :last-stream-id ,last-stream-id
                           :error-code ,error-code)))

(defmethod do-goaway :around ((connection logging-object) error-code last-stream-id debug-data)
  (call-next-method))

;;;; network comm simplifications
(defmethod close ((connection http2-connection) &key &allow-other-keys)
  (close (get-network-stream connection)))


