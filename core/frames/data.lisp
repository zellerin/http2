(in-package http2/core)

(defgeneric apply-data-frame (stream data start end))
(defgeneric apply-window-size-increment (stream increment))

(defclass flow-control-mixin ()
  ((window-size      :accessor get-window-size      :initarg :window-size)
   (peer-window-size :accessor get-peer-window-size :initarg :peer-window-size))
  (:documentation
   "The flow control parameters that are kept both per-stream and per-connection."))

(define-frame-type 0 :data-frame
    "```
  +---------------+-----------------------------------------------+
  |                            Data (*)                         ...
  +---------------------------------------------------------------+
```

  DATA frames (type=0x0) convey arbitrary, variable-length sequences of
  octets associated with a stream.  One or more DATA frames are used,
  for instance, to carry HTTP request or response payloads."
    ((data (or cons vector null)))
    (:length (if (consp data) (reduce #'+ data :key #'length) (length data))
     :flags (padded end-stream)
     :must-have-stream-in (open half-closed/local))
    (lambda (buffer start data)
      (account-write-window-contribution
       (get-connection stream) stream length)
      (if (consp data)
          (dolist (chunk data)
            (replace buffer chunk :start1 start)
            (incf start (length chunk)))
          (replace buffer data :start1 start)))

    (lambda (connection data active-stream flags)
      "Read octet vectors from the stream and call APPLY-DATA-FRAME on them.

Reduce tracked incoming window.

Run PEER-ENDS-HTTP-STREAM callback on the stream if appropriate."
      (with-padding-marks (connection flags start end)
        (account-read-window-contribution connection active-stream (- end start))
        (apply-data-frame active-stream data start end)
        (maybe-end-stream flags active-stream)
        (values #'parse-frame-header 9))))

(defun account-read-window-contribution (connection stream length)
  ;; TODO: throw an error when this goes below zero
  (decf (get-window-size connection) length)
  (decf (get-window-size stream) length))

(defun account-write-window-contribution (connection stream length)
  (decf (get-peer-window-size connection) length)
  (decf (get-peer-window-size stream) length))

(defun write-priority (priority buffer start &optional headers)
  (unless (null (cdr headers))
    (error "FIXME: not implemented "))
  (write-31-bits buffer start
                 (priority-stream-dependency priority)
                 (priority-exclusive priority))
  (setf (aref buffer (+ 4 start)) (priority-weight priority))
  (replace buffer (car headers) :start1 (+ start 5))
  buffer)

(define-frame-type 8 :window-update-frame
    "```
    +-+-------------------------------------------------------------+
    |R|              Window Size Increment (31)                     |
    +-+-------------------------------------------------------------+
  ```

The WINDOW_UPDATE frame (type=0x8) is used to implement flow control; see
Section 5.2 for an overview.  Flow control operates at two levels: on each
individual stream and on the entire connection."
    ((window-size-increment 31))
    (:length 4
     :has-reserved t
     ;; The spec does not limit states list, but idle does not make sense.
     :must-have-stream-in (open closed half-closed/local half-closed/remote
                                reserved/local reserved/remote)
     :may-have-connection t)
    (lambda (buffer start window-size-increment reserved)
      (write-31-bits buffer start  window-size-increment reserved)
      (incf (get-window-size stream-or-connection) window-size-increment))

    (lambda (connection data http-stream flags)
      "Update window information for outgoing data and invoke
APPLY-WINDOW-SIZE-INCREMENT callback."
      (unless (zerop flags)
        (warn "Flags for windows size increment set: ~x/~b" flags flags))
      (unless (= 4 (length data))
        (connection-error 'incorrect-window-update-frame-size connection))
      (let ((window-size-increment (aref/wide data 0 4)))
        (when (plusp (ldb (byte 1 31) window-size-increment))
          (warn "Reserved bit in WINDOW-UPDATE-FRAME is set"))
        (cond
          ((plusp window-size-increment)
           (apply-window-size-increment http-stream (ldb (byte 31 0) window-size-increment)))
          ((eq connection http-stream)
           (connection-error 'null-connection-window-update connection))
          (t
           (http-stream-error 'null-stream-window-update http-stream))))
      (values #'parse-frame-header 9)))
