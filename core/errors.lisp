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

(define-condition too-big-frame (connection-error)
  ()
  (:default-initargs :code +frame-size-error+)
  (:documentation
   "Frame exceeds the size defined in SETTINGS_MAX_FRAME_SIZE."))

(define-condition too-big-padding (connection-error)
  ()
  (:default-initargs :code +protocol-error+)
  (:documentation
   "Length of the padding is the length of the frame payload or greater."))

(define-condition our-id-created-by-peer (connection-error)
  ()
  (:default-initargs :code +protocol-error+))

(define-condition frame-type-needs-stream (connection-error)
  ()
  (:default-initargs :code  +protocol-error+)
  (:documentation
   "Frame MUST be associated with a stream. If a frame is received whose
    stream identifier field is 0x0, the recipient MUST respond with a
    connection error (Section 5.4.1) of type PROTOCOL_ERROR."))

(define-condition bad-stream-state (connection-error)
  ((allowed :accessor get-allowed :initarg :allowed)
   (actual  :accessor get-actual  :initarg :actual))
  (:documentation
   "Frame cannot be applied to stream in particular state"))

(define-condition http-stream-error (error)
  ((code   :accessor get-code   :initarg :code)
   (stream :accessor get-stream :initarg :stream)))

(defmethod print-object ((err http-stream-error) out)
  (with-slots (error-code debug-data stream) err
      (print-unreadable-object (err out :type t)
        (format out "~a: ~a" stream (get-error-name error-code)))))



(defun http-stream-error (e stream)
  (error (make-instance e :stream stream)))

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

(define-condition unexpected-continuation-frame (connection-error)
  ()
  (:default-initargs :code +protocol-error+)
  (:documentation
   "A CONTINUATION frame MUST be preceded by a HEADERS, PUSH_PROMISE or
   CONTINUATION frame without the END_HEADERS flag set.  A recipient that
   observes violation of this rule MUST respond with a connection error (Section
   5.4.1) of type PROTOCOL_ERROR."))
