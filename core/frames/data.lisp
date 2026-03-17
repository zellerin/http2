(in-package http2/core)

(declaim (optimize (speed 3) (safety 1)))

;;;; content:
;;;; - flow control frame definition
;;;; - classes
;;;; - reading
;;;;  - higher level functions to read
;;;;  - low level
;;;; - writing
;;;;  - higher level function
;;;;  - low level
;;;; - data frame definition

(defsection @flow-control (:title "Flow control")
  "Each (non-empty) data frame consumes part of the data window. The accounting for
the windows is maintained by the FLOW-CONTROL-MIXIN."
  (flow-control-mixin class)
  (get-peer-window-size generic-function)
  (apply-window-size-increment generic-function)
  (long-write function)
  (available-window-size function))

(declaim (ftype (function (flow-control-mixin) window-size) get-peer-window-size))

(defclass flow-control-mixin ()
  ((window-size      :accessor get-window-size      :initarg :window-size
                     :type window-size)
   (peer-window-size :accessor get-peer-window-size :initarg :peer-window-size
                     :type window-size)
   (window-size-increment-callback :accessor get-window-size-increment-callback :initarg :window-size-increment-callback))
  (:default-initargs :window-size-increment-callback nil)
  (:documentation
   "The flow control parameters that are kept both per-stream and per-connection.

In addition to the accounting items (current window size of both endpoints) it
also has an output buffer and tracks a callback to be called when window is
increased (WINDOW-SIZE-INCREMENT-CALLBACK)."))

(defun available-window-size (http-stream &optional (connection (get-connection http-stream)))
  "Smaller of connection and stream window size. You should not send in the data
frame for the stream more than this."
  (min (get-peer-window-size connection) (get-peer-window-size http-stream)))

(defsection @buffered ()
  "BUFFERED-STREAM mixin implements flow control on the senders side.

It accepts new data with WRITE-OCTET-TO-STREAM and WRITE-SEQUENCE-TO-STREAM.

FLUSH-STREAM-BUFFER uses generic functions WRITE-BUFFERED-FRAME to pass the
data and signals further."
  (buffered-stream class)
  (write-octet-to-stream function)
  (write-sequence-to-stream function)
  (flush-stream-buffer function)
  (send-available-data function))

(defvar *default-stream-buffer-size* 65536
  "Buffer size for buffers. Default is chosen same as the default initial frame
size for buffers, which is 65536.")

(declaim (ftype (function (t) (integer 0 #.array-dimension-limit)) get-flush-mark)
         (ftype (function (t) octet-vector) get-output-buffer))

(defclass buffered-stream (flow-control-mixin)
  ((output-buffer :accessor get-output-buffer :initarg :output-buffer
                  :documentation "Data to send, window permitting.")
   (flush-mark    :accessor get-flush-mark    :initarg :flush-mark
                  :type fixnum
                  :documentation "Data up to FLUSH-MARK are flushed, i.e., they should be sent even when less than
a full frame. Still, write can be delayed due to insufficient window.")
   (to-close      :accessor get-to-close      :initarg :to-close))
  (:documentation
   "Hold queue of data to write in a buffer. Some of the data (flushed) are to be
sent as soon as possible given flow control constrains, the rest is to be send
when it is efficient (to prevent small frames).")
  (:default-initargs :output-buffer
                     (make-array *default-stream-buffer-size* :element-type '(unsigned-byte 8)
                                                              :fill-pointer 0 :adjustable nil)
                     :flush-mark -1 :to-close nil))

(defgeneric write-buffered-frame (http-stream buffer offset size end-stream)
  ;; this is split out to allow override for debugging and testing
  (:documentation "Send a single buffered frame to HTTP-STREAM.")
  (:method (stream buffer offset size end-stream)
    (write-data-frame-region stream buffer offset size :end-stream end-stream)))

(defun send-available-data (http-stream)
  "Send queued data to the peer, respecting the peer window size limit and frame size efficiency.

Specifically, send data while either
- they fit full frame and full frame window is open,
- there are data to flush and they fit the window,  "
  (declare (type http2-stream http-stream))
  (loop
    with available-window of-type window-size = (available-window-size http-stream)
    and peer-frame-size = (get-max-peer-frame-size (get-connection http-stream))
    and buffer = (get-output-buffer http-stream)
    and offset of-type fixnum = 0
    and flush-mark = (get-flush-mark http-stream)
    with fill-pointer = (fill-pointer buffer)
    for tentative-size = (min peer-frame-size (- fill-pointer offset)) ; how much can we write in one frame
    while (and (>= available-window peer-frame-size) ; window allows to write full frame
               (or (> flush-mark offset)             ; do not buffer flush data
                   (= tentative-size peer-frame-size)))
    do
       (write-buffered-frame http-stream buffer offset tentative-size
                             (and (get-to-close http-stream)
                                  (= fill-pointer (+ offset tentative-size))))
       (incf offset tentative-size)
       (decf available-window offset)
    finally
       (when (plusp offset)
         (replace buffer buffer :start1 0 :start2 offset)
         (setf (fill-pointer buffer) (- fill-pointer offset))
         (setf (get-flush-mark http-stream) (max -1 (- (get-flush-mark http-stream) offset)))
         (flush-http2-data (get-connection http-stream)))
       (return (>= available-window peer-frame-size))))

(defmacro with-buffer-slots (stream &body body)
  `(with-slots (connection peer-window-size state output-buffer) ,stream
     ,@body))

(defvar *buffer-grow-limit* (* 16 65536))

(defun maybe-grow-buffer (buffer &optional (target 0))
  (cond
    ((> target *buffer-grow-limit*)
     (cerror "Ok" "Too much data written and waiting."))
    ((>= target (array-dimension buffer 0))
     (adjust-array buffer (max target
                               (* 2 (array-dimension buffer 0)))))))

(defun write-octet-to-stream (stream byte)
  "Write an octet to the output buffer.

Special cases:

- Buffer is full -> extend it
- Buffer contains more that max peer frame size octets -> send the data out "
  (with-buffer-slots stream
    (maybe-grow-buffer output-buffer (fill-pointer output-buffer))
    (vector-push byte output-buffer)
    (when (>= (fill-pointer output-buffer)
              (get-max-peer-frame-size connection))
      (send-available-data stream))))

(defun write-sequence-to-stream (stream sequence start end)
  "Write an octet to the output buffer, possibly extending it and sending out.

- Buffer is full -> extend it
- Buffer contains more that max peer frame size octets -> send the data out "
  (with-buffer-slots stream
    (let* ((old-fill (fill-pointer output-buffer))
           (new-fill (+ (- end start) old-fill)))
      (maybe-grow-buffer output-buffer new-fill)
      (setf (fill-pointer output-buffer) new-fill)
      (replace output-buffer sequence :start1 old-fill :start2 start
                                      :end2 end))
    (send-available-data stream)))

(defun flush-stream-buffer (stream end-stream-p)
  (with-buffer-slots stream
    (setf (get-flush-mark stream) (fill-pointer (get-output-buffer stream))
          (get-to-close stream) end-stream-p)
    (send-available-data stream)))

(define-condition duplicit-long-write ()
  ((old-action :accessor get-old-action :initarg :old-action)
   (new-action :accessor get-new-action :initarg :new-action)
   (stream     :accessor get-stream     :initarg :stream)
   (requested  :accessor get-requested  :initarg :requested))
  (:report (lambda (error out)
             (with-slots (old-action new-action stream requested) error
               (with-slots (peer-window-size) stream
                 (format out "~@<~W requests long write ~W, but we already have a callback action ~W~:_~:>"
                         stream new-action old-action))))))

(defun continue-long-write (stream action size-needed)
  "Write chunks of possibly long data to HTTP/2 stream.

Run chain of ACTIONS till the window size is too small or next action is NIL."
  (declare (type flow-control-mixin stream)
           ((or null (and compiled-function (function () ))) action))
  (with-slots (peer-window-size window-size-increment-callback connection) stream
    (with-slots ((connection-window peer-window-size)) connection
      (loop
        (cond ((null action)
               (setf window-size-increment-callback nil)
               (return nil))
              ((> size-needed peer-window-size)
               (setf window-size-increment-callback action)
               (return action))
              ((> size-needed connection-window)
               (error "Connection window too small"))
              (t (setf (values action size-needed) (funcall action))))))))

(defun long-write (stream action size-needed)
  "Write chunks of possibly long data to HTTP/2 stream.

ACTION writes data to the stream using appropriate layer (not
necessary flushing), and return two values, next ACTION and size needed for
it. Writing to the stream also implicitly lowers the peer-window-size.

Calling the chain of these function is done either till the next action is NIL,
or till the peer window size is too low. In the latter case, store the next
function so that this is re-run when the appropriate window frame is received.

This is supposed to be final part of sending data to the stream. Do not call it second
time on same stream.

See /long example in demo.lisp."
  (declare (type http2-stream stream)
           (compiled-function action))
  (with-slots (window-size-increment-callback) stream
    (when (and action window-size-increment-callback)
      (error 'window-full :old-action window-size-increment-callback :new-action action
                          :stream stream)))
  (continue-long-write stream action size-needed))

(defgeneric apply-window-size-increment (object increment)
  (:documentation
   "Called on window update frame. By default, increases PEER-WINDOW-SIZE slot of
the stream or connection, and possibly calls WINDOW-SIZE-INCREMENT-CALLBACK.")
  (:method ((object (eql :closed)) increment))
  (:method ((object flow-control-mixin) increment)
    (with-slots (window-size-increment-callback peer-window-size) object
      (incf peer-window-size increment)
      ;; FIXME: If a sender receives a WINDOW_UPDATE that causes a flow-control window
      ;; to exceed this maximum, it MUST terminate either the stream or the
      ;; connection, as appropriate. For streams, the sender sends a RST_STREAM
      ;; with an error code of FLOW_CONTROL_ERROR; for the connection, a GOAWAY
      ;; frame with an error code of FLOW_CONTROL_ERROR is sent.
      (when window-size-increment-callback
        (continue-long-write object window-size-increment-callback (get-max-peer-frame-size (get-connection object))))))
#+nil
  (:method ((object buffered-stream) increment)
    (with-slots (window-size-increment-callback peer-window-size) object
      (incf peer-window-size increment)
      (when (and (send-available-data object) window-size-increment-callback)
        (funcall window-size-increment-callback)))))

(defun account-read-window-contribution (connection stream length)
  "Update window size when we receive data."
  ;; TODO: throw an error when this goes below zero
  (decf (get-window-size connection) length)
  (decf (get-window-size stream) length))

(defun account-write-window-contribution (connection stream length)
  "Update peer window size for stream and connection after writing data.

Do not check whether write is possible."
  (decf (get-peer-window-size connection) length)
  (decf (get-peer-window-size stream) length))

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
  "User in most cases probably prefers not to care about windows and data frames,
and binary vs text and content encoding and compression of the data.

This poses several topics:

- Overlay a Common Lisp stream over data frames. This is done using gray
  streams, see XXX.
-  Handle window management. The problem is that we do not
  want to block an individual stream handler. So the options I can see are:

- We can actually ignore the issue, if the size of the output is small, it would
  work. You can WRITE-DATA-FRAME if you need to send binary data up to 16384 octets (or more
  if negotiated with the per)
- We can signal error when the output is too big, and let the application code handle that.
  The application would need a callback to resume operations when the window is open again.
  Problem - it complicates the application.
- We can buffer all the input and send it when there is an opportunity automatically.
  Problem - potentially conses a lot."
  "Starting from the low-level, options are:"
  )

(defsection @data-classes (:title "Classes")
  )

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

(defun write-data-frame (stream data &key padded end-stream (start 0) (length (length data)))
  "```
  +---------------+-----------------------------------------------+
  |                            Data (*)                         ...
  +---------------------------------------------------------------+
```

  DATA frames (type=0x0) convey arbitrary, variable-length sequences of
  octets associated with a stream.  One or more DATA frames are used,
  for instance, to carry HTTP request or response payloads."
  (write-data-frame-region stream data start length :padded padded :end-stream end-stream))

(defun write-data-frame-region (stream data start length &rest keys &key padded end-stream)
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
  (write-vector-frame stream  +data-frame+ keys data start length)
  (account-write-window-contribution (get-connection stream)
                                     stream length))

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
