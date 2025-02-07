
(in-package http2/core)

(defsection @frame-headers
    ()
  (add-header generic-function)
  (process-end-headers generic-function)
  (header-collecting-mixin class)
  (get-headers (method nil header-collecting-mixin)))

(defclass header-collecting-mixin ()
  ((headers :accessor get-headers :initarg :headers
            :documentation "List of collected (header . value) pairs. Does not include `:method`, `:path`, etc."))
  (:default-initargs :headers nil)
  (:documentation
   "Mixin to be used to collect all observed headers to a slot."))


(defgeneric add-header (connection stream name value)
  (:method (connection stream name value)
    #+nil    (warn 'no-new-header-action :header name :stream stream))

  (:method (connection (stream server-stream) (name symbol) value)
    (when (get-seen-text-header stream)
      (http-stream-error 'pseudo-header-after-text-header stream
                         :name name
                         :value value))
    (case name
      (:method
          (check-place-empty-and-set-it value get-method))
      (:scheme
       (check-place-empty-and-set-it value get-scheme))
      (:authority
       (check-place-empty-and-set-it value get-authority))
      (:path
       (check-place-empty-and-set-it value get-path))
      (t
       (http-stream-error 'incorrect-request-pseudo-header  stream
                          :name name :value value))))

  (:method (connection (stream client-stream) (name symbol) value)
    (when (get-seen-text-header stream)
      (http-stream-error 'pseudo-header-after-text-header stream
                         :name name
                         :value value))
    (case name
      (:status
       (setf (get-status stream) value))
      (t
       (http-stream-error 'incorrect-response-pseudo-header stream
                          :name name
                          :value value))))

  (:method (connection (stream header-collecting-mixin) name value)
    (push (cons name value) (get-headers stream))))

(defgeneric process-end-headers (connection stream)

  (:method (connection stream))

  (:method (connection (stream client-stream))
    ;; Headers sanity check
    (unless (get-status stream)
      (http-stream-error 'missing-pseudo-header stream
                         :name :status
                         :value 'missing))
    ;; next header section may contain another :status
    (setf (get-seen-text-header stream) nil))

  (:method (connection (stream server-stream))
    ;; Headers sanity check
    (unless (or (equal (get-method stream) "CONNECT")
             (and (get-method stream) (get-scheme stream) (get-path stream)))
      (http-stream-error 'missing-pseudo-header stream
                         :name "One of pseudo headers"
                         :value 'missing))))

(defgeneric apply-stream-priority (stream exclusive weight stream-dependency)
  (:method  (stream exclusive weight stream-dependency)
    (setf (get-weight stream) weight
          (get-depends-on stream)
          `(,(if exclusive :exclusive :non-exclusive) ,stream-dependency)))
  (:documentation
   "Called when priority frame - or other frame with priority settings set -
arrives. Does nothing, as priorities are deprecated in RFC9113 anyway."))

(defclass hpack-endpoint ()
  ((compression-context      :accessor get-compression-context      :initarg :compression-context)
   (decompression-context    :accessor get-decompression-context    :initarg :decompression-context))
  (:default-initargs :compression-context (make-instance 'hpack-context)
                     :decompression-context (make-instance 'hpack-context)))

(defstruct priority
  "Structure capturing stream priority parameters." exclusive stream-dependency weight)

(define-frame-type 1 :headers-frame
    "```

    +-+-------------+-----------------------------------------------+
    |E|                 Stream Dependency? (31)                     |
    +-+-------------+-----------------------------------------------+
    |  Weight? (8)  |
    +-+-------------+-----------------------------------------------+
    |                   Header Block Fragment (*)                 ...
    +---------------------------------------------------------------+
```

   The HEADERS frame (type=0x1) is used to open a stream (Section 5.1),
   and additionally carries a header block fragment.  HEADERS frames can
   .be sent on a stream in the \"idle\", \"reserved (local)\", \"open\", or
   \"half-closed (remote)\" state."
    ((headers list))                    ;  &key dependency weight
    (:length (+ (if priority 5 0) (reduce '+ (mapcar 'length headers)))
     :flags (padded end-stream end-headers
              ;; PRIORITY is both flag and prio content  to write
              priority)
     :must-have-stream-in (open idle reserved/remote half-closed/local)
     :new-stream-state open)

    ;; writer
    nil
    ;; reader
    (lambda (connection data active-stream flags)
      "Read incoming headers and call ADD-HEADER callback for each header.

Call PROCESS-END-HEADERS and PEER-ENDS-HTTP-STREAM (in this order) if relevant
flag is set.

At the beginning, invoke APPLY-STREAM-PRIORITY if priority was present."
      (handler-case
          (with-padding-marks (connection flags start end)
            (when (get-flag flags :priority)
              (read-priority data active-stream start)
              (incf start 5)
              (when (< start end)
                (connection-error 'frame-too-small-for-priority connection)))
            (read-and-add-headers data active-stream start end flags flags))

        (http-stream-error (e)
          (format t "-> We close a stream due to ~a" e)
          (values #'parse-frame-header 9)))))

(defun write-simple-headers-frame
       (stream headers &rest keys &key end-headers end-stream)
  "No priority, no padding.
    +-+-------------+-----------------------------------------------+
    |                   Header Block Fragment (*)                 ...
    +---------------------------------------------------------------+
```

   The HEADERS frame (type=0x1) is used to open a stream (Section 5.1),
   and additionally carries a header block fragment.  HEADERS frames can
   .be sent on a stream in the \"idle\", \"reserved (local)\", \"open\", or
   \"half-closed (remote)\" state."
  (declare (ignore end-stream end-headers))
  (let ((length (length headers)))
    (write-frame stream length +headers-frame+ keys
                 (lambda (buffer start headers)
                   (replace buffer headers :start1 start))
                 headers)))

(defun write-headers-frame
    (stream headers &rest keys &key end-headers end-stream padded priority)
  "```

    +-+-------------+-----------------------------------------------+
    |E|                 Stream Dependency? (31)                     |
    +-+-------------+-----------------------------------------------+
    |  Weight? (8)  |
    +-+-------------+-----------------------------------------------+
    |                   Header Block Fragment (*)                 ...
    +---------------------------------------------------------------+
```

   The HEADERS frame (type=0x1) is used to open a stream (Section 5.1),
   and additionally carries a header block fragment.  HEADERS frames can
   .be sent on a stream in the \"idle\", \"reserved (local)\", \"open\", or
   \"half-closed (remote)\" state."
  (declare (ignore padded end-stream end-headers))
  (let ((length
          (+
           (if priority
               5
               0)
           (length headers))))
    (write-frame stream length +headers-frame+ keys
                 (lambda (buffer start headers priority)
                   (if priority
                       (write-priority priority buffer start headers)
                       (replace buffer headers :start1 start)))
                 headers priority)))

(defun read-and-add-headers (data http-stream start end flags header-flags)
  "Read http headers from payload in DATA, starting at START.

Returns next function to call and size of expected data. If the END-HEADERS was
present, it would be PARSE-FRAME-HEADER, if not a continuation frame is to be
read.

Note that END-STREAM is present in the first header flag, not in the
continuation flags, if any, so must be separate."
  ;; FIXME: add handler for failure to read full header.
  (let* ((connection (get-connection http-stream))
         (end-headers (get-flag flags :end-headers))
         (to-backtrace
           (do-decoded-headers (lambda (name value)
                                 (add-header connection http-stream name value))
             (get-compression-context connection) data start end)))
    (cond
      ((and end-headers to-backtrace)
       ;; 20240718 TODO: make class for this connection error
       (error "Incomplete headers: ~a" (subseq data to-backtrace end)))
      (end-headers
       (process-end-headers connection http-stream)
       (maybe-end-stream header-flags http-stream)
       (values #'parse-frame-header 9))
      (to-backtrace
       (values (read-continuation-frame-on-demand http-stream data to-backtrace end header-flags)
               9))
      (t
       (error "TODO: Implement me nicely")))))

(defun read-priority (data http-stream start)
  (let* ((e+strdep (aref/wide data start 4))
         (weight (aref data (+ start 4)))
         (exclusive (plusp (ldb (byte 1 31) e+strdep)))
         (stream-dependency (ldb (byte 31 0) e+strdep)))
    (apply-stream-priority http-stream exclusive weight stream-dependency)
    (values #'parse-frame-header 9)))

#|
Prioritization information in a HEADERS frame is logically equivalent ;
to a separate PRIORITY frame, but inclusion in HEADERS avoids the ;
potential for churn in stream prioritization when new streams are ;
created.  Prioritization fields in HEADERS frames subsequent to the ;
first on a stream reprioritize the stream (Section 5.3.3). ;
                                        ;
|#
(define-frame-type 2 :priority-frame
    "
   The PRIORITY frame (type=0x2) specifies the sender-advised priority
   of a stream ([Section 5.3](https://www.rfc-editor.org/rfc/rfc9113.html#name-prioritization)).

```
    +-+-------------------------------------------------------------+
    |E|                  Stream Dependency (31)                     |
    +-+-------------+-----------------------------------------------+
    |   Weight (8)  |
    +-+-------------+
```

   The payload of a PRIORITY frame contains the following fields:

   E: A single-bit flag indicating that the stream dependency is
      exclusive.

   Stream Dependency:  A 31-bit stream identifier for the stream that
      this stream depends on.

   Weight:  An unsigned 8-bit integer representing a priority weight for
      the stream.  Add one to the value to obtain a
      weight between 1 and 256."
    ((priority priority))
    (:length 5
     :must-have-stream-in (idle reserved/remote reserved/local open half-closed/local half-closed/remote closed)
     :new-stream-state idle)
    (lambda (buffer start priority)
      (write-priority priority buffer start))
    (lambda (connection data http-stream flags)
      "Read priority frame. Invoke APPLY-STREAM-PRIORITY if priority was present."
      (declare (ignore connection))
      (assert (zerop start))
      (unless (= 5 length)
        ;;   A PRIORITY frame with a length other than 5 octets MUST be treated as
        ;;   a stream error (Section 5.4.2) of type FRAME_SIZE_ERROR.
        (http-stream-error 'frame-size-error http-stream))
      (read-priority data http-stream 0)))

(define-frame-type 9 :continuation-frame
    "```

    +---------------------------------------------------------------+
    |                   Header Block Fragment (*)                 ...
    +---------------------------------------------------------------+
```

The CONTINUATION frame (type=0x9) is used to continue a sequence of header block
fragments (Section 4.3).  Any number of CONTINUATION frames can be sent, as long
as the preceding frame is on the same stream and is a HEADERS, PUSH_PROMISE, or
CONTINUATION frame without the END_HEADERS flag set."
    ((headers list))
    (:length (reduce '+ (mapcar 'length headers))
     :flags (end-headers end-stream))
    (lambda (buffer start headers)
      (when (cdr headers)
        (error "Multiple header groups not supported now."))
      (replace buffer (car headers) :start1 start))

    ;; reader
    ;; If we needed continuation frame, we would be in READ-BYTE*.
    ;; TODO: Confirm and verify, otherwise require state CONTINUATION-NEEDED.
    (lambda (connection data http-stream flags)
      "Raise condition CONNECTION-ERROR. The continuation frame should be read only when
expected, and then it is parsed by a different function."
      (declare (ignore data start length))
      (connection-error 'unexpected-continuation-frame connection)))

(defun read-continuation-frame-on-demand (expected-stream old-data old-data-start old-data-end header-flags)
  ;; this is pretty much copy of READ-FRAME-HEADER, but with a twists
  (values
   (lambda (connection header &optional (start 0) (end (length header)))
     (declare
      ((simple-array (unsigned-byte 8) *) header old-data)
      ((integer 0 #.array-dimension-limit) start end))
     (assert (= 9 (- end start)))
     (multiple-value-bind (frame-type-object length flags http-stream R)
         (decode-frame-header header start)
       (let* ((expected-stream-id (get-stream-id expected-stream)))
         (declare ((unsigned-byte 24) length old-data-end old-data-start)
                  ((unsigned-byte 8) flags)
                  (stream-id http-stream expected-stream-id)
                  (ftype (function (t) (unsigned-byte 24)) get-max-frame-size))
         (when (> length (the (unsigned-byte 24) (get-max-frame-size connection)))
           ;; fixme: sometimes connection error.
           (connection-error 'too-big-frame connection
                             :frame-size length
                             :max-frame-size (get-max-frame-size connection)))
         (if (plusp R) (warn 'reserved-bit-set))
         (unless (eq (frame-type-name frame-type-object) :continuation-frame)
           ; FIXME: make it an object
           (error "Expected continuation frame and got ~a, should handle this"
                  (frame-type-name frame-type-object)))
         (unless (= http-stream expected-stream-id)
           (error "Expected continuation for ~d, got ~d" http-stream
                  expected-stream-id))
         ;; Note: we use headers-frame for this...
         (let* ((stream-or-connection expected-stream))
           (cond
             ((and (get-flag flags :end-headers) (zerop length))
              ;; empty HEADER-FRAME still can have end-streams or end-headers flag
              (read-and-add-headers old-data  stream-or-connection
                                   old-data-start old-data-end flags header-flags))
             ((zerop length)
              ;; empty continuation header, expect another one
              (read-continuation-frame-on-demand expected-stream old-data old-data-start old-data-end header-flags))
             (t
              (values (lambda (connection data)
                        (declare (ignore connection))
                        (declare ((simple-array (unsigned-byte 8) *) data))
                        (let ((full-data (make-octet-buffer (+ length
                                                               (- old-data-end old-data-start)))))
                          (unless (= old-data-start old-data-end)
                            (replace full-data old-data :start2 old-data-start
                                                        :end2 old-data-end))
                          (replace full-data data :start1 (- old-data-end old-data-start))
                          (read-and-add-headers full-data stream-or-connection
                                                0 (length full-data) flags header-flags)))
                      length)))))))
   9))
