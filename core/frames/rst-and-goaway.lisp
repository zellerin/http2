(in-package http2/core)

(defgeneric peer-resets-stream (stream reason))

(defgeneric do-goaway (connection error-code last-stream-id debug-data)
  (:documentation
   "Called when a go-away frame is received. By default throws GO-AWAY condition if
error was reported.")
  (:method (connection error-code last-stream-id debug-data)
    (unless (eq error-code '+no-error+)
      (error 'go-away :last-stream-id last-stream-id
                      :error-code error-code
                      :debug-data debug-data))))

(define-frame-type 3 :rst-stream-frame
    "The RST_STREAM frame (type=0x3) allows for immediate termination of a
   stream.  RST_STREAM is sent to request cancellation of a stream or to
   indicate that an error condition has occurred.

```
    +---------------------------------------------------------------+
    |                        Error Code (32)                        |
    +---------------------------------------------------------------+
```

   The RST_STREAM frame contains a single unsigned, 32-bit integer
   identifying the error code (Section 7).  The error code indicates why
   the stream is being terminated.

   The RST_STREAM frame does not define any flags."
    ((error-code 32))
    (:length 4
     ;;    RST_STREAM frames MUST NOT be sent for a stream in the \"idle\" state.
     ;;    If a RST_STREAM frame identifying an idle stream is received, the
     ;;    recipient MUST treat this as a connection error (Section 5.4.1) of
     ;;    type PROTOCOL_ERROR.
     :must-have-stream-in (reserved/remote reserved/local open half-closed/local half-closed/remote closed)
     :bad-state-error +protocol-error+)
    (lambda (buffer start code)
      (setf (aref/wide buffer start 4) code))
    (lambda (connection data http-stream flags)
      "Invoke PEER-RESETS-STREAM callback."
      (assert (zerop flags))
      (assert (zerop start))
      (unless (= length 4)
        (connection-error 'incorrect-rst-frame-size connection))
      (peer-resets-stream http-stream (aref/wide data 0 4))
      (values #'parse-frame-header 9)))

(defun http-stream-error (e stream &rest args)
  "We detected a HTTP2-STREAM-ERROR in a peer frame. So we send a RST frame, raise appropriate warning in case someone is interested, close affected stream, and continue."
  (let ((e (apply #'make-instance e :stream stream args)))
    (unless (eql stream :closed)
      (write-rst-stream-frame stream (get-code e))
#+nil      (force-output (get-network-stream stream)))
    (warn e)
    (close-http2-stream stream)))

(define-frame-type 7 :goaway-frame
    "```
    +-+-------------------------------------------------------------+
    |R|                  Last-Stream-ID (31)                        |
    +-+-------------------------------------------------------------+
    |                      Error Code (32)                          |
    +---------------------------------------------------------------+
    |                  Additional Debug Data (*)                    |
    +---------------------------------------------------------------+
   ```

   The GOAWAY frame (type=0x7) is used to initiate shutdown of a
   connection or to signal serious error conditions.  GOAWAY allows an
   endpoint to gracefully stop accepting new streams while still
   finishing processing of previously established streams.  This
   enables administrative actions, like server maintenance."
    ((last-stream-id 31)
     (error-code 32)
     (debug-data vector))
    (:length (+ 8 (length debug-data))
     :must-have-connection t
     :has-reserved t)

    (lambda (buffer start last-stream-id error-code debug-data reserved)
      (write-31-bits buffer start last-stream-id reserved)
      (setf
       (aref/wide buffer (+ start 4) 4) error-code)
      (replace buffer debug-data :start1 (+ start 8))
      buffer)

    ;; reader
    (lambda (connection data http-stream flags)
      "Invoke DO-GOAWAY callback."
      (declare (ignore length))
      (unless (zerop flags) (warn "Flags set for goaway frame: ~d" flags))
      (assert (zerop start))
      (let ((last-id (aref/wide data 0 4))
            (error-code (aref/wide data 4 4))
            (data (subseq data 8)))
        (do-goaway connection (get-error-name error-code) last-id data))
      (invoke-restart 'close-connection)))

(defun connection-error (class connection &rest args)
  "Send \\GOAWAY frame to the PEER and raise the CONNECTION-ERROR[condition].
NETWORK-STREAM used."
  (let ((err (apply #'make-condition class :connection connection args)))
    (with-slots (code) err
      (write-goaway-frame connection
                          0             ; fixme: last processed stream
                          code
                          (map 'vector 'char-code (symbol-name class))))
    (error err)))
