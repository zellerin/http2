(in-package :http2)

#+deleteme
(define-condition peer-should-go-away (serious-condition)
  ((error-code     :accessor get-error-code     :initarg :error-code)
   (debug-data     :accessor get-debug-data     :initarg :debug-data)
   (last-stream-id :accessor get-last-stream-id :initarg :last-stream-id))
  (:documentation
   "Signalled when we have sent goaway frame and want to close connection."))

(define-condition go-away (serious-condition)
  ((error-code     :accessor get-error-code     :initarg :error-code)
   (debug-data     :accessor get-debug-data     :initarg :debug-data)
   (last-stream-id :accessor get-last-stream-id :initarg :last-stream-id))
  (:documentation "Signaleed when GO-AWAY frame received."))

(defmethod print-object ((err go-away) out)
  (with-slots (error-code debug-data last-stream-id) err
    (print-unreadable-object (err out :type t)
      (format out "~a (~s)" error-code (map 'string 'code-char debug-data)))))

(define-condition connection-error (error)
  ((connection :accessor get-connection :initarg :connection)
   (code       :accessor get-code       :initarg :code))
  (:documentation
   "A connection error is signalled when we detect an illegal frame content.

Typically it is signalled from CONNECTION-ERROR function that also sends appropriate GOAWAY frame. After that it should be handled by server or client as needed - server may close connection, client may retry the request."))

(defun connection-error (class connection &rest args)
  (let ((err (apply #'make-instance class :connection connection args)))
    (with-slots (code) err
      (write-goaway-frame connection
                          0             ; fixme: last processed stream
                          code
                          (map 'vector 'char-code (symbol-name class))))
    (finish-output (get-network-stream connection))
    (error err)))

(defmethod print-object ((ce connection-error) out)
  (print-unreadable-object (ce out :type t)
    (format out "on ~a" (get-connection ce))))

(define-condition protocol-error (connection-error)
  ()
  (:default-initargs :code +protocol-error+))

(define-condition too-big-frame (connection-error)
  ((max-frame-size :accessor get-max-frame-size :initarg :max-frame-size)
   (frame-size     :accessor get-frame-size     :initarg :frame-size))
  (:default-initargs :code +frame-size-error+)
  (:documentation
   "Frame exceeds the size defined in SETTINGS_MAX_FRAME_SIZE."))

(define-condition too-big-padding (protocol-error)
  ()
  (:documentation
   "Length of the padding is the length of the frame payload or greater."))

(define-condition our-id-created-by-peer (protocol-error)
  ())

(define-condition frame-type-needs-stream (protocol-error)
  ()
  (:documentation
   "Frame MUST be associated with a stream. If a frame is received whose
    stream identifier field is 0x0, the recipient MUST respond with a
    connection error (Section 5.4.1) of type PROTOCOL_ERROR."))

(define-condition new-stream-id-too-low (protocol-error)
  ((stream-id       :accessor get-stream-id       :initarg :stream-id)
   (max-seen-so-far :accessor get-max-seen-so-far :initarg :max-seen-so-far))
  (:documentation
   "The identifier of a newly established stream MUST be numerically
      greater than all streams that the initiating endpoint has opened or
      reserved (max was ~d).  This governs streams that are opened using a
      HEADERS frame and streams that are reserved using PUSH_PROMISE.  An
      endpoint that receives an unexpected stream identifier MUST respond with a
      connection error (Section 5.4.1) of type PROTOCOL_ERROR."))

(define-condition incorrect-enable-push-value (protocol-error)
  ((value :accessor get-value :initarg :value))
  (:documentation "Client must have ENABLE-PUSH 0 or 1. Server must have ENABLE-PUSH 0."))

(define-condition incorrect-frame-size-value (protocol-error)
  ((value :accessor get-value :initarg :value))
  (:documentation
   "Frame size MUST be between the initial value 16384 and the maximum allowed frame
size (2^24-1 or 16,777,215 octets), inclusive."))

(define-condition incorrect-initial-window-size-value (protocol-error)
  ((value :accessor get-value :initarg :value))
  (:documentation
   "SETTINGS_INITIAL_WINDOW_SIZE must be below 2^31."))

(define-condition bad-stream-state (connection-error)
  ((allowed   :accessor get-allowed   :initarg :allowed)
   (actual    :accessor get-actual    :initarg :actual)
   (stream-id :accessor get-stream-id :initarg :stream-id))
  (:documentation
   "Frame cannot be applied to stream in particular state"))

(define-condition http-stream-error (warning)
  ((code   :accessor get-code   :initarg :code)
   (stream :accessor get-stream :initarg :stream)))

(defmethod print-object ((err http-stream-error) out)
  (with-slots (stream) err
      (print-unreadable-object (err out :type t)
      #+nil  (format out "on ~a" stream))))

(defun http-stream-error (e stream &rest args)
  "We detected an error on peer stream. So we send a RST frame, warn in case someone in interested, and go on."
  (let ((e (apply #'make-instance e :stream stream args)))
    (unless (eql stream :closed)
      (write-rst-stream-frame stream (get-code e) )
      (force-output (get-network-stream stream)))
    (warn e)
    (close-http2-stream stream)))

(define-condition incorrect-frame-size (http-stream-error)
  ()
  (:default-initargs :code +frame-size-error+)
  (:documentation
   "A PRIORITY frame with a length other than 5 octets MUST be treated as a stream error (Section 5.4.2) of type FRAME_SIZE_ERROR."))

(define-condition incorrect-rst-frame-size (connection-error)
  ()
  (:default-initargs :code +frame-size-error+)
  (:documentation
   "A RST_STREAM frame with a length other than 4 octets MUST be treated as a connection error (Section 5.4.1) of type FRAME_SIZE_ERROR."))

(define-condition incorrect-settings-frame-size (connection-error)
  ()
  (:default-initargs :code +frame-size-error+)
  (:documentation
   "A SETTINGS frame with a length other than a multiple of 6 octets MUST be treated as a connection error (Section 5.4.1) of type FRAME_SIZE_ERROR."))

(define-condition incorrect-ping-frame-size (connection-error)
  ()
  (:default-initargs :code +frame-size-error+)
  (:documentation
   "Receipt of a PING frame with a length field value other than 8 MUST be treated as a connection error (Section 5.4.1) of type FRAME_SIZE_ERROR."))

(define-condition incorrect-window-update-frame-size (connection-error)
  ()
  (:default-initargs :code +frame-size-error+)
  (:documentation
   "Receipt of a PING frame with a length field value other than 8 MUST be treated as a connection error (Section 5.4.1) of type FRAME_SIZE_ERROR."))

(define-condition unexpected-continuation-frame (protocol-error)
  ()
  (:documentation
   "A CONTINUATION frame MUST be preceded by a HEADERS, PUSH_PROMISE or
   CONTINUATION frame without the END_HEADERS flag set.  A recipient that
   observes violation of this rule MUST respond with a connection error (Section
   5.4.1) of type PROTOCOL_ERROR."))

(define-condition stream-protocol-error (http-stream-error)
  ()
  (:default-initargs :code +protocol-error+))

(define-condition header-error (stream-protocol-error)
  ((name :accessor get-name :initarg :name)
   (value  :accessor get-value  :initarg :value)))

(define-condition incorrect-pseudo-header (header-error)
  ())

(define-condition pseudo-header-after-text-header (header-error)
  ()
  (:documentation "Pseudo header follows text header."))

(define-condition incorrect-response-pseudo-header (header-error)
  ())

(define-condition incorrect-request-pseudo-header (header-error)
  ())

(define-condition duplicate-request-header (header-error)
  ())

(define-condition lowercase-header-field-name (header-error)
  ()
  (:documentation
   "A request or response containing uppercase header field names MUST be treated
   as malformed. (...) Malformed requests or responses that are detected MUST be
   treated as a stream error of type PROTOCOL_ERROR."))

(define-condition missing-pseudo-header (header-error)
  ()
  (:documentation
   ":status pseudo-header field MUST be included in all responses.

 All HTTP/2 requests MUST include exactly one valid value for the
   :method, :scheme, and :path pseudo-header fields, unless it is
   a CONNECT request."))

(define-condition null-stream-window-update (http-stream-error)
  ()
  (:default-initargs :code +protocol-error+)
  (:documentation
   "A receiver MUST treat the receipt of a WINDOW_UPDATE frame with a flow-control window increment of 0 as a stream error."))

(define-condition null-connection-window-update (protocol-error)
  ()
  (:documentation
   "Errors on the connection flow-control window MUST be treated as a connection error."))

(define-condition missing-header-octets (protocol-error)
  ()
  (:documentation "We try to process headers, but end up in a middle of one."))
