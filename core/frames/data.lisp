(in-package http2/core)

(defsection @data
    ()
  (get-peer-window-size generic-function)
  (get-max-peer-frame-size generic-function)
  (write-data-frame-multi function))

(defclass body-collecting-mixin ()
  ((body :accessor get-body :initarg :body
         :documentation "Body of the request as an octet vector.

May be empty if some higher priority mixin (e.g., UTF8-PARSER-MIXIN) processed
the data."))
  (:default-initargs :body nil)
  (:documentation
   "Mixin to collect all payload parts to one string."))

(defgeneric apply-data-frame (stream payload start end)
  (:documentation
   "STREAM (a HTTP/2 stream) should process received PAYLOAD from the data
frame from START to END.")

  ;; FIXME: we should not send small updates

  (:method (stream payload start end)
    "Just ignore the data and warn about it."
    (warn 'no-payload-action :class stream))

  (:method ((stream body-collecting-mixin) data start end)
    "Concatenate received data to the BODY slot of the object."
    (setf (get-body stream)
          (concatenate '(vector (unsigned-byte 8))
                       (get-body stream)
                       (subseq data start end) ))
    (with-slots (connection) stream
      (write-window-update-frame connection (length data))
      (write-window-update-frame stream (length data)))))

(defclass flow-control-mixin ()
  ((window-size      :accessor get-window-size      :initarg :window-size)
   (peer-window-size :accessor get-peer-window-size :initarg :peer-window-size)
   (window-open-fn   :accessor get-window-open-fn   :initarg :window-open-fn
                     :initform nil))
  (:documentation
   "The flow control parameters that are kept both per-stream and per-connection."))

(defmethod get-peer-window-size ((dummy (eql :closed)))
  "No data to send to closed peer, so make it 0.

This is used only in tracing, not during normal use."
  0)

(defgeneric apply-window-size-increment (object increment)
  (:documentation
   "Called on window update frame. By default, increases PEER-WINDOW-SIZE slot of
the strem or connection.")
  (:method ((object (eql :closed)) increment))
  (:method (object increment)
    (incf (get-peer-window-size object) increment))
  (:method :after ((object flow-control-mixin) increment)
    (with-slots (window-open-fn) object
      (when window-open-fn
        (funcall window-open-fn)))))

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
    nil
    (lambda (connection data active-stream flags)
      "Read octet vectors from the stream and call APPLY-DATA-FRAME on them.

Reduce tracked incoming window.

Run PEER-ENDS-HTTP-STREAM callback on the stream if appropriate."
      (assert (zerop start))
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

(defun write-data-frame (stream data &rest keys &key padded end-stream)
  "```
  +---------------+-----------------------------------------------+
  |                            Data (*)                         ...
  +---------------------------------------------------------------+
```

  DATA frames (type=0x0) convey arbitrary, variable-length sequences of
  octets associated with a stream.  One or more DATA frames are used,
  for instance, to carry HTTP request or response payloads."
  (declare (ignore padded end-stream)
           (octet-vector data))
  (let ((length (length data)))
    (write-frame stream length +data-frame+ keys
                 (lambda (buffer start data)
                   (account-write-window-contribution (get-connection stream)
                                                      stream length)
                   (replace buffer data :start1 start))
                 data)))

(defun write-data-frame-multi (stream data &rest keys &key end-stream)
  "Write a data frame that includes DATA - that is a sequence of octet vectors."
  (declare (ignore end-stream))
  (let ((length (reduce #'+  data :key #'length)))
    (write-frame stream length +data-frame+ keys
                 (lambda (buffer start data)
                   (account-write-window-contribution (get-connection stream)
                                                      stream length)
                   (dolist (datum data)
                     (replace buffer datum :start1 start)
                     (incf start (length datum))))
                 data)))

#+maybe-not-needed
(defun write-data-list-frame (stream data &rest keys &key padded end-stream)
  "```
  +---------------+-----------------------------------------------+
  |                            Data (*)                         ...
  +---------------------------------------------------------------+
```

  DATA frames (type=0x0) convey arbitrary, variable-length sequences of
  octets associated with a stream.  One or more DATA frames are used,
  for instance, to carry HTTP request or response payloads."
  (declare (ignore padded end-stream))
  (let ((length
          (if (consp data)
              (reduce #'+ data :key #'length)
              (length data))))
    (write-frame stream length 0 keys
                 (lambda (buffer start data)
                   (account-write-window-contribution (get-connection stream)
                                                      stream length)
                   (if (consp data)
                       (dolist (chunk data)
                         (replace buffer chunk :start1 start)
                         (incf start (length chunk)))
                       (replace buffer data :start1 start)))
                 data)))


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
      (unless (= (+ start 4) length)
        (connection-error 'incorrect-window-update-frame-size connection))
      (let ((window-size-increment (aref/wide data start 4)))
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

(defsection @tracing-windows (:title "Debugging window events")
  "You can run TRACE-PEER-WINDOW or TRACE-WINDOW to trace window change events. On
sbcl it looks like

```
(http2/core:trace-peer-window)
(handler-bind ((warning 'muffle-warning)) ; retrieve-url on example.com warns normally
   (http2/client:retrieve-url \"https://www.example.com\")
  (values))
.. (>0) Processed 1256 octets on Half-Closed/Local stream #1
.. (>0) Window update size sent on #<VANILLA-CLIENT-CONNECTION >, from 64279 by 1256
.. (>0) Window update size sent on Half-Closed/Local stream #1, from 64279 by 1256
..
```

As always, use untrace to stop tracing."
  (trace-window function)
  (trace-peer-window function))

(defun trace-peer-window ()
  "Trace events related to the windows that peer has to set data:

- when we send out the window update frame (window increases), and
- when we receive data (windows decreases)."
  (trace-object  write-window-update-frame 0
      ("Window update size sent on ~a, from ~d by ~d"
       (& 0) (get-window-size (& 0)) (& 1)))
  (trace-object apply-data-frame 0 ("Processed ~a octets on ~a" (- (& 3) (& 2))
                                                                (& 0))))

(defun trace-window ()
  "Trace events related to the window we have to send data:

- When we send data, and
- when we process window size frame callback"
  (trace-object apply-window-size-increment 0
      ("~s updates window size from ~d by ~d" (& 0) (get-peer-window-size (& 0))
                                              (& 1)))
  (trace-object write-data-frame 0 ("~d octets of data to write to ~a, my window ~a"
                                    (length (& 1)) (& 0)
                                    (get-peer-window-size (& 0))))
  (trace-object write-data-frame-multi 0 ("~d octets of data (multi) to write to ~a, my window ~a"
                                    (reduce #'+ (& 1) :key #'length) (& 0)
                                    (get-peer-window-size (& 0))))
  (trace-object (method set-peer-setting (t (eql :initial-window-size) t))
      0
      ("Peer on ~a set window size to ~a" (& 0) (& 2))))
