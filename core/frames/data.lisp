(in-package http2/core)

;;;; content:
;;;; - classes
;;;; - flow control frame definition
;;;; - reading
;;;;  - higher level functions to read
;;;;  - low level
;;;; - writing
;;;;  - higher level function
;;;;  - low level
;;;; - data frame definition


(defsection @data (:title "Data and flow control")
  "HTTP streams can receive some data (content, body). We can see treat in
several ways:

- we can collect the data till all is received (end of stream from the peer) and
  then process it as whole. If you need this, use BODY-COLLECTING-MIXIN class and GET-BODY function, or
  TEXT-COLLECTING-STREAM class and HTTP-STREAM-TO-STRING function.
- we may want to do something with each recieved chunk of data as soon as it
  arrives. In this case, specialize APPLY-DATA-FRAME or APPLY-TEXT-DATA-FRAME.

In both cases, one may want to work either with a string or with octets. The
latter is default, for the former use UTF8-PARSER-MIXIN and/or
FALLBACK-ALL-IS-ASCII.

The body can be gzipped; in such case derive the stream from GZIP-DECODING-MIXIN."
  (apply-data-frame generic-function)
  (apply-text-data-frame generic-function)
  (apply-data-frame (method (utf8-parser-mixin t t t)))
  (body-collecting-mixin class)
  (apply-data-frame (method  (BODY-COLLECTING-MIXIN t t t)))
  (utf8-parser-mixin class)
  (is-utf8-p function)
  (gzip-decoding-mixin class)
  (apply-data-frame (method :around (gzip-decoding-mixin t t t)))
  (text-collecting-stream class)
  "Data to write are send by WRITE-DATA-FRAME-MULTI or WRITE-DATA-FRAME. These do
not take into account limits set up by the peer, so use WRITE-BINARY-PAYLOAD instead."
  "Send and received octets are accounted for and must be within some limits (window)."
  (flow-control-mixin class)
  (get-peer-window-size generic-function)
  (get-max-peer-frame-size generic-function))

(defsection @data-classes (:title "Classes")
  )

(defclass flow-control-mixin ()
  ((window-size      :accessor get-window-size      :initarg :window-size)
   (peer-window-size :accessor get-peer-window-size :initarg :peer-window-size)
   (window-open-fn   :accessor get-window-open-fn   :initarg :window-open-fn
                     :initform nil))
  (:documentation
   "The flow control parameters that are kept both per-stream and per-connection."))

(defclass body-collecting-mixin ()
  ((body :accessor get-body :initarg :body
         :documentation "Body of the request as an octet vector.

May be empty if some higher priority mixin (e.g., UTF8-PARSER-MIXIN) processed
the data."))
  (:default-initargs :body nil)
  (:documentation
   "Mixin to collect all payload parts to one string."))

(defclass text-collecting-stream ()
  ((text :accessor get-text :initarg :text))
  (:default-initargs :text nil)
  (:documentation
   "Mixin that collect all the received body (possibly unzipped data frames
converted to proper encoding) into its TEXT slot."))

(defun http-stream-to-string (http-stream)
  "HTTP-STREAM should be a TEXT-COLLECTING-STREAM.

HTTP-STREAM-TO-VECTOR then assembles the text from individual chunks."
  (with-output-to-string (*standard-output*)
    (mapc 'princ (nreverse (get-text http-stream)))))

(defclass multi-part-data-stream ()
  ((window-size-increment-callback :accessor get-window-size-increment-callback :initarg :window-size-increment-callback))
  (:default-initargs :window-size-increment-callback nil)
  (:documentation
   "Implement writing of data that may possibly be too big to send at once.

When peer sends window size increment frame, call specified callback
function. This is set in WRITE-BINARY-PAYLOAD to write rest of data to write."))

(defclass constant-output-stream (trivial-gray-streams:fundamental-binary-output-stream http2/stream-overlay::binary-stream)
  ((output-buffer :accessor get-output-buffer))
  (:default-initargs :to-write 0 :to-store 0)
  (:documentation
   "Binary stream that accepts new octets to the output-buffer"))

(defun parse-window-update-frame (http-stream flags)
  (declare (ignorable http-stream flags))
  (values
   (lambda (connection data &optional (start 0) (length (length data)))
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
          (apply-window-size-increment http-stream
                                       (ldb (byte 31 0)
                                            window-size-increment)))
         ((eq connection http-stream)
          (connection-error 'null-connection-window-update connection))
         (t (http-stream-error 'null-stream-window-update http-stream))))
     (values #'parse-frame-header 9))))

(defmethod get-peer-window-size ((dummy (eql :closed)))
  "No data to send to closed peer, so make it 0.

This is used only in tracing, not during normal use."
  0)

(defsection @accepting-data
    (:title "Accepting data frames")
  "When a data frame is received, APPLY-DATA-FRAME generic function is called. The specific
action depends on the class of the receiving HTTP/2 stream.")

(defun account-read-window-contribution (connection stream length)
  "Update window size when we receive data."
  ;; TODO: throw an error when this goes below zero
  (decf (get-window-size connection) length)
  (decf (get-window-size stream) length))

(defun parse-data-frame (active-stream flags)
  "Return parser for a generic data frame (possibly with a padding). That is a
function of type PARSER-FN."
  (declare (ignorable active-stream flags))
  (values
   (lambda (connection data &optional (start 0) (length (length data)))
     "Read octet vectors from the stream and call APPLY-DATA-FRAME on them.

Reduce tracked incoming window.

Run PEER-ENDS-HTTP-STREAM callback on the stream if appropriate."
     (with-padding-marks (connection flags start end)
       (account-read-window-contribution connection active-stream
                                         (- end start))
       (apply-data-frame active-stream data start end)
       (maybe-end-stream flags active-stream)
       (values #'parse-frame-header 9)))))



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

(defmethod apply-text-data-frame ((stream text-collecting-stream) text)
  (push text (get-text stream)))



(defsection @data-write (:title "Writing"))

(defun account-write-window-contribution (connection stream length)
  "Update peer window size for stream and connection after writing data.

Do not check whether write is possible."
  (decf (get-peer-window-size connection) length)
  (decf (get-peer-window-size stream) length))

(defun write-binary-payload (connection stream payload &key (end-stream t))
  "Write binary PAYLOAD to the http2 STREAM.

The payload is written in chunks of peer frame size, and if the available window
is not big enough we stop writing and return, making sure that the writing
continues when window size increases."
  (let ((sent 0)
        (total-length (length payload)))
    (with-slots (window-size-increment-callback) stream
      (flet ((write-chunk (stream)
               (loop
                 for allowed-window = (min (get-peer-window-size connection)
                                           (get-peer-window-size stream))
                 and frame-size = (get-max-peer-frame-size connection)
                 and data-to-send = (- total-length sent)
                 for have-full-frame-to-send = (>= data-to-send frame-size)
                 and allowed-send-full-frame = (>= allowed-window frame-size)
                 while (and have-full-frame-to-send allowed-send-full-frame)
                 do
                    (write-data-frame stream (subseq payload sent (+ frame-size sent))
                                      :end-stream nil)
                    (incf sent frame-size)
                 finally (when (>= allowed-window data-to-send)
                           (write-data-frame stream (subseq payload sent)
                                             :end-stream end-stream)
                           (setf window-size-increment-callback nil)))))
        #+nil        (when window-size-increment-callback
                       (error "FIXME: this is unsupported, do we really need :END-STREAM nil version?"))
        (setf window-size-increment-callback #'write-chunk)
        (write-chunk stream)))))

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

(defmethod apply-window-size-increment :after ((object multi-part-data-stream) increment)
  (with-slots (window-size-increment-callback) object
    (when window-size-increment-callback
      (funcall window-size-increment-callback object))))

(defsection @lisp-stream-emulation
    (:title "Emulate Lisp stream over frames")

  "TODO: sort docs below"
  (compile-payload-from-stream dref:macro)
  (write-binary-payload function)
  (make-transport-output-stream function)
  (multi-part-data-stream class)
  (http-stream-to-string function)
  (@accepting-data section))

(defmethod initialize-instance :after ((stream constant-output-stream) &key &allow-other-keys)
  (setf (get-output-buffer stream)
        (make-array 1024 :element-type '(unsigned-byte 8)
                         :fill-pointer 0 :adjustable t)))

(defmethod trivial-gray-streams:stream-write-byte ((stream constant-output-stream) byte)
  (with-slots (output-buffer) stream
    (vector-push-extend byte output-buffer)))

(defmacro compile-payload-from-stream ((stream-name charset gzip) &body body)
  "Run BODY with STREAM-NAME bound to a stream named STREAM-NAME. Return octets
that represent text written to that stream given specified CHARSET, possibly
compressed.

```cl-transcript
(http2/core:compile-payload-from-stream (foo :utf8 nil) (princ \"Hello😃\" foo))
=> #(72 101 108 108 111 240 159 152 131)
```
"
  `(let* ((transport (make-instance 'constant-output-stream))
          (base transport))
     (when ,gzip
       (setf transport (gzip-stream:make-gzip-output-stream transport)))
     (awhen ,charset
       (setf transport
             (flexi-streams:make-flexi-stream
              transport
              :external-format ,charset)))
     (with-open-stream (,stream-name transport)
       ,@body
       (get-output-buffer base))))

(defun make-transport-output-stream (http2-stream charset gzip)
  "An OUTPUT-STREAM built atop HTTP2-STREAM with possible translating CHARSET to
octets and compression (if GZIP set)."
  (let* ((transport (make-instance 'http2/stream-overlay::payload-output-stream :base-http2-stream http2-stream)))
    (when gzip
      (setf transport (gzip-stream:make-gzip-output-stream transport)))
    (when charset
      (setf transport
            (flexi-streams:make-flexi-stream
             transport
             :external-format charset)))
    transport))

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

(defsection @tracing-windows (:title "Debugging window events")
  "You can run TRACE-PEER-WINDOW or TRACE-WINDOW to trace window change events. On
sbcl it looks like

```
(http2/core:trace-peer-window)
(handler-bind ((warning 'muffle-warning)) ; retrieve-url on example.com warns normally
   (http2/client:retrieve-url *example-url*)
  (values))
.. (>0) Processed 1256 octets on Half-Closed/Local stream #1
.. (>0) Window update size sent on #<VANILLA-CLIENT-CONNECTION >, from 64279 by 1256
.. (>0) Window update size sent on Half-Closed/Local stream #1, from 64279 by 1256
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

(defsection @data-frame-lowlevel (:title "Data frame")
  (parse-data-frame function)
  (write-data-frame function)
  (write-data-frame-multi function))

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
    nil)

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

(define-frame-writer 8 write-window-update-frame stream-or-connection nil 4
  ((window-size-increment 31)) ((reserved t)) "```
    +-+-------------------------------------------------------------+
    |R|              Window Size Increment (31)                     |
    +-+-------------------------------------------------------------+
  ```

The WINDOW_UPDATE frame (type=0x8) is used to implement flow control; see
Section 5.2 for an overview.  Flow control operates at two levels: on each
individual stream and on the entire connection."
  (lambda (buffer start window-size-increment reserved)
    (write-31-bits buffer start window-size-increment
                   reserved)
    (incf (get-window-size stream-or-connection)
          window-size-increment)))

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
    nil
    nil)
