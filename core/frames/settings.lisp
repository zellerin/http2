(in-package http2/core)

(defgeneric peer-acks-settings (connection))
(defgeneric set-peer-setting (connection name value))
(defgeneric peer-expects-settings-ack (connection))

(define-frame-type 4 :settings-frame
    "```
    +-------------------------------+
    |       Identifier (16)         |
    +-------------------------------+-------------------------------+
    |                        Value (32)                             |
    +---------------------------------------------------------------+
   ```

   The SETTINGS frame (type=0x4) conveys configuration parameters that
   affect how endpoints communicate, such as preferences and constraints
   on peer behavior.  The SETTINGS frame is also used to acknowledge the
   receipt of those parameters.  Individually, a SETTINGS parameter can
   also be referred to as a \"setting\"."
    ((settings list))
    (:length (* (length settings) 6)
     :must-have-connection t
     :flags (ack))
    ;; writer
    (lambda (buffer start settings)
      (loop
        for i from start by 6
        and setting in settings
        do
           (setf (aref/wide buffer i 2)
                 (if (numberp (car setting)) (car setting)
                     (find-setting-code (car setting)))
                 (aref/wide buffer (+ i 2) 4)
                 (cdr setting))
        finally (return buffer)))
    ;;reader
    (lambda (connection data http-stream flags)

      "Parse settings frame. If this is ACK settings frame, invoke PEER-ACKS-SETTINGS
callback, otherwise invoke SET-PEER-SETTING callback for each setting in the
recieved order."
      (let ((length (length data)))
        (cond
          ((get-flag flags :ack)
           (when (plusp length)
             (error "Ack settings frame must be empty. We should close connection."))
           (peer-acks-settings connection))
          (t
           (unless (zerop (mod length 6))
             (connection-error 'incorrect-settings-frame-size connection))
           (loop
             for idx from 0 to (1- length) by 6
             for identifier = (aref/wide data idx 2)
             and value = (aref/wide data (+ 2 idx) 4)
             for name = (find-setting-by-id identifier)
             ;;    An endpoint that receives a SETTINGS frame with any unknown or
             ;;    unsupported identifier MUST ignore that setting.
             when name
               do (set-peer-setting connection name value)
             finally (peer-expects-settings-ack connection)))))
      (values #'parse-frame-header 9)))

(defun write-ack-setting-frame (connection)
  "Write ACK settings frame.

   ACK (0x1):  When set, bit 0 indicates that this frame acknowledges
      receipt and application of the peer's SETTINGS frame.  When this
      bit is set, the payload of the SETTINGS frame MUST be empty.
      Receipt of a SETTINGS frame with the ACK flag set and a length
      field value other than 0 MUST be treated as a connection error
      (Section 5.4.1) of type FRAME_SIZE_ERROR.  For more information,
      see Section 6.5.3 (\"Settings Synchronization\")."
  (write-frame connection 0 +settings-frame+ '(:ack t) nil))
