(in-package :http2/core)

;;;; Error codes
(eval-when (:compile-toplevel :load-toplevel)
  (defvar *error-codes*
    (macrolet ((defcode (name code documentation)
                 `(progn
                    (defconstant ,name ,code ,documentation))))
      (vector
       (defcode +no-error+            0  "The associated condition is not a result of an error")
       (defcode +protocol-error+      1  "The endpoint detected an unspecific protocol error")
       (defcode +internal-error+      2  "The endpoint encountered an unexpected internal error")
       (defcode +flow-control-error+  3  "The endpoint detected that its peer violated the flow-control protocol")
       (defcode +settings-timeout+    4  "The endpoint sent a SETTINGS frame but did not receive a response in a timely manner")
       (defcode +stream-closed+       5  "The endpoint received a frame after a stream was half-closed")
       (defcode +frame-size-error+    6  "The endpoint received a frame with an invalid size")
       (defcode +refused-stream+      7  "The endpoint refused the stream prior to performing any application processing")
       (defcode +cancel+              8  "The endpoint indicates that the stream is no longer needed")
       (defcode +compression-error+   9  "The endpoint is unable to maintain the header compression context for the connection")
       (defcode +connect-error+       #xa  "The connection established in response to a      CONNECT request was reset or abnormally closed")
       (defcode +enhance-your-calm+   #xb  "The endpoint detected that its peer is exhibiting a behavior that might be generating excessive load")
       (defcode +inadequate-security+ #xc  "The underlying transport has properties that do not meet minimum security requirements")
       (defcode +http-1-1-required+   #xd  "The endpoint requires that HTTP/11 be used instead of HTTP/2")))

    "This table maps error codes to mnemonic names - symbols.

   Error codes are 32-bit fields that are used in RST_STREAM and GOAWAY
   frames to convey the reasons for the stream or connection error.

   Error codes share a common code space.  Some error codes apply only
   to either streams or the entire connection and have no defined
   semantics in the other context."))

(defun get-error-name (code)
  ;; This is used only by the error printer
  "Get HTTP/2 error name from the error code.

```cl-transcript
(http2/core::get-error-name 1)
=> HTTP2/CORE::+PROTOCOL-ERROR+

(http2/core::get-error-name 33)
=> HTTP2/CORE::UNDEFINED-ERROR-CODE-21
```"
  (if (<= 0 code #xd)
      (aref *error-codes* code)
      (values (intern (format nil "UNDEFINED-ERROR-CODE-~x" code) 'http2/core))))

(defsection @errors
    (:title "Errors handlers")
  (http2-simple-error condition)
  (http2-simple-warning condition)
  (connection-error condition)
  (connection-error function)
  (http-stream-error condition)
  (http-stream-error function)
  (too-big-frame condition)
  (frame-too-small-for-priority condition)
  (go-away condition)
  (do-goaway generic-function)
  (h2-not-supported-by-server condition))

(define-condition go-away (serious-condition)
  ((error-code     :accessor get-error-code     :initarg :error-code)
   (debug-data     :accessor get-debug-data     :initarg :debug-data)
   (last-stream-id :accessor get-last-stream-id :initarg :last-stream-id))
  (:report (lambda (err stream)
             (with-slots (error-code debug-data) err
               (format stream
                       "~a sent GO-AWAY frame with error ~a.~@[~2%There were debug data in frame:~%~a~]"
                       (get-medium err)
                       error-code (and debug-data (map 'string 'code-char debug-data)))))))

(define-condition http2-condition ()
  ())

(define-condition http2-error (http2-condition error)
  ()
  (:documentation "All errors raised from HTTP2 package inherit from this error."))

(defmethod describe-object :before ((error http2-condition) out)
  (write-line (documentation (class-of error) t) out)
  (terpri out))

(define-condition http2-simple-error (http2-error simple-condition)
  ())

(defmethod print-object ((err go-away) out)
  (with-slots (error-code debug-data last-stream-id) err
    (print-unreadable-object (err out :type t)
      (format out "~a (~s)" error-code (map 'string 'code-char debug-data)))))

(define-condition connection-error (http2-error)
  ((connection :accessor get-connection :initarg :connection)
   (code       :accessor get-code       :initarg :code))
  (:documentation
   "A connection error is signalled when we detect an illegal frame content.

Use [CONNECTION-ERROR][function] function to signal it or its
subclasses. Application must handle it, including closing associated
NETWORK-STREAM."))

(defmethod print-object ((ce connection-error) out)
  (print-unreadable-object (ce out :type t)
    (format out "on ~a" (get-connection ce))))

(define-condition protocol-error (connection-error)
  ()
  (:default-initargs :code +protocol-error+))

(define-condition client-preface-mismatch (protocol-error)
  ((received :accessor get-received :initarg :received))
  (:documentation "HTTPS server expects a specific sequence of octets at the start of the new
connection. The client sent something different."))

(defmethod print-object ((err client-preface-mismatch) out)
  (with-slots (received) err
    (print-unreadable-object (err out :type t)
      (format out "~a ~a" received  (map 'string 'code-char received)))))

(define-condition too-big-frame (connection-error)
  ((max-frame-size :accessor get-max-frame-size :initarg :max-frame-size)
   (frame-size     :accessor get-frame-size     :initarg :frame-size))
  (:default-initargs :code +frame-size-error+)
  (:documentation
   "Frame exceeds the size defined in SETTINGS_MAX_FRAME_SIZE."))

(defmethod print-object ((o too-big-frame) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "Frame size 0x~x, max ~x." (get-frame-size o) (get-max-frame-size o))))

(define-condition too-big-padding (protocol-error)
  ()
  (:documentation
   "Length of the padding is the length of the frame payload or greater."))

(define-condition frame-too-small-for-priority (protocol-error)
  ()
  (:documentation
   "Length of the padding is the length of the frame payload or greater."))

(define-condition our-id-created-by-peer (protocol-error)
  ())

(define-condition frame-type-needs-stream (protocol-error)
  ((frame-type :accessor get-frame-type :initarg :frame-type))
  (:documentation
   "Frame MUST be associated with a stream. If a frame is received whose
    stream identifier field is 0x0, the recipient MUST respond with a
    connection error (Section 5.4.1) of type PROTOCOL_ERROR."))

(define-condition frame-type-needs-connection (protocol-error)
  ((frame-type :accessor get-frame-type :initarg :frame-type))
  (:documentation
   "Frame type must be applied on connection."))

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

(define-condition incorrect-setting-value (protocol-error)
  ((setting-code :accessor get-setting-code :initarg :setting-code)
   (value        :accessor get-value        :initarg :value)
   (allowed      :accessor get-allowed      :initarg :allowed)))

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
  (with-slots (stream code) err
      (print-unreadable-object (err out :type t)
        (format out "~d (~a) on ~s"
                (get-error-name code)
                (documentation (get-error-name code) 'variable)
                stream))))

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

(define-condition unsupported-feature (warning)
  ())

(define-condition reserved-bit-set (unsupported-feature)
  ()
  (:documentation "Reserved bit is set in received frame header. We ignore it."))

(defsection @warnings ())

(define-condition http2-warning (http2-condition warning)
  ())

(define-condition http2-simple-warning (simple-condition http2-warning) ())

(define-condition implement-by-user (http2-warning simple-condition)
  ()
  (:documentation "Something that the HTTP2 library expects to be implemented by server/client."))

(define-condition unimplemented-feature (http2-warning simple-condition)
  ()
  (:documentation "Something that can be implemented to better match RFC suggestions or that we are obliged to ignore"))

(define-condition no-payload-action (http2-warning)
  ((class :accessor get-class :initarg :class))
  (:documentation "No payload action defined, and payload received."))

(defmethod print-object ((o no-payload-action) out)
  (print-unreadable-object (o out :type t)
    (format out "No payload action defined for ~a" (class-name (class-of (get-class o))))))

(define-condition no-new-header-action (http2-warning)
  ((header :accessor get-header :initarg :header)
   (stream :accessor get-stream :initarg :stream))
  (:documentation "Header that could not be handled arrived."))

(define-condition h2-not-supported-by-server (error)
  ((host :accessor get-host :initarg :host)
   (port :accessor get-port :initarg :port)))
