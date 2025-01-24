(in-package http2/core)

(defgeneric do-ping (connection data)
  (:documentation
   "Called when ping-frame without ACK is received.
By default send ping-frame with ACK and same data.")
  (:method (connection data)
    (write-ping-frame connection data :ack t)))

(defgeneric do-pong (connection data)
  (:documentation
   "Called when ping-frame with ACK is received. By default warns about unexpected ping response; see also TIMESHIFT-PINGING-CONNECTION mixin.")
  (:method (connection data)
    (warn 'implement-by-user
          :format-control "The connection ~a did not expect ping response"
          :format-arguments (list connection))))

(defun do-ping-fn (connection data &optional (start 0) (end (length data)))
  (assert (zerop start))
  (unless (= (- end start) 8)
    (connection-error 'incorrect-ping-frame-size connection))
  (do-ping connection (aref/wide data start 8))
  (values #'parse-frame-header 9))

(defun do-pong-fn (connection data &optional (start 0) (end (length data)))
  (assert (zerop start))
  (unless (= (- end start) 8)
    (connection-error 'incorrect-ping-frame-size connection))
  (do-pong connection (aref/wide data start 8))
  (values #'parse-frame-header 9))

(defun parse-ping-frame (http-stream flags)
  (declare (ignorable http-stream)
           ((unsigned-byte 8) flags))
  (values
   (if (get-flag flags :ack)
       #'do-pong-fn
       #'do-ping-fn)))

(define-frame-type 6 :ping-frame
    "The PING frame (type=0x6) is a mechanism for measuring a minimal
   round-trip time from the sender, as well as determining whether an
   idle connection is still functional.  PING frames can be sent from
   any endpoint.

   ```
    +---------------------------------------------------------------+
    |                                                               |
    |                      Opaque Data (64)                         |
    |                                                               |
    +---------------------------------------------------------------+
   ```

   In addition to the frame header, PING frames MUST contain 8 octets of
   opaque data in the payload.  A sender can include any value it
   chooses and use those octets in any fashion.

   Receivers of a PING frame that does not include an ACK flag MUST send
   a PING frame with the ACK flag set in response, with an identical
   payload.  PING responses SHOULD be given higher priority than any
   other frame.

   The PING frame defines the following flags:

   ACK (0x1):  When set, bit 0 indicates that this PING frame is a PING
      response.  An endpoint MUST set this flag in PING responses.  An
      endpoint MUST NOT respond to PING frames containing this flag."
    ((opaque-data 64))
    (:length 8 :flags (ack)
     :must-have-connection t)
    ;; writer
    nil
    nil)

(defun write-ping-frame (connection opaque-data &rest keys &key ack)
   "The PING frame (type=0x6) is a mechanism for measuring a minimal
    round-trip time from the sender, as well as determining whether an
    idle connection is still functional.  PING frames can be sent from
    any endpoint.

    ```
     +---------------------------------------------------------------+
     |                                                               |
     |                      Opaque Data (64)                         |
     |                                                               |
     +---------------------------------------------------------------+
    ```

    In addition to the frame header, PING frames MUST contain 8 octets of
    opaque data in the payload.  A sender can include any value it
    chooses and use those octets in any fashion.

    Receivers of a PING frame that does not include an ACK flag MUST send
    a PING frame with the ACK flag set in response, with an identical
    payload.  PING responses SHOULD be given higher priority than any
    other frame.

    The PING frame defines the following flags:

    ACK (0x1):  When set, bit 0 indicates that this PING frame is a PING
       response.  An endpoint MUST set this flag in PING responses.  An
       endpoint MUST NOT respond to PING frames containing this flag."
   (declare (ignore ack))
   (let ((length 8))
     (write-frame connection length 6 keys
                  (lambda (buffer start opaque-data)
                    (setf (aref/wide buffer start 8) opaque-data)
                    buffer)
                  opaque-data)))
