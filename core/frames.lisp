;;;; Copyright 2022, 2023, 2024 by Tomáš Zellerin

(in-package :http2)

(defsection @frames-api
  (:title "API for sending and receiving frames")
  "![image](../frames.png) There are several main low-level components:

- @FRAME-HANDLERS that reads data from some source (typically, a socket) related
  to connection, sets up environment to allow callbacks to write data, calls
  appropriate parsing functions, and writes collected responses,
- @FRAME-PARSERS, that parses the frame header and payload octets, and calls respective callbacks,
- @CALLBACKS that implement application specific behaviour,
- @FRAME-WRITERS such as WRITE-DATA-FRAME that are typically invoked from the callbacks,
- @ERRORS that break the loop as needed.
 "
  (@frame-handler section)
  (@frame-parsers section)
  (@callbacks section)
  (@frame-writers section)
  (@old-frame-functions section)
  (@errors section))

(defsection @frame-handler
    (:title "Frame handler")
  "Frame handler implements data moving from outside world to the connection. The generic structure is:

- set current frame parser to an initial one (e.g., PARSE-FRAME-HEADER)  and expected length to expected (e.g., 9)
- loop:
   - set up a space to write data in the connection
   - wait for some data to arrive (or obtain them somehow)
   - process as much data as possible by calling current parser to obtain next one
- end on an error, or when there are no longer any data"
  "Existing handlers include PROCESS-PENDING-FRAMES."
  "FIXME: currently the end is either by catching END-OF-FILE, or by invoking restart FINISH-STREAM. Document what is *correct*.")

(defsection @frame-parsers
    (:title "Frame parsers")
  "Frame parser take an octet of data of expected size, process it (calling any
callback applicable) and return two values, next frame parser and expected size.

The entry point is PARSE-FRAME-HEADER."
  (parse-frame-header function)
  (parse-data-frame function)
  (parse-headers-frame function)
  (parse-priority-frame function)
  (parse-rst-stream-frame function)
  (parse-settings-frame function)
  (parse-push-promise-frame function)
  (parse-ping-frame function)
  (parse-goaway-frame function)
  (parse-window-update-frame function)
  (parse-continuation-frame function)
  (parse-altsvc-frame function))

(defsection @frame-writers
    (:title "Frame writing functions")
  "The write function are expected to be called either as part of initializing
communication (READ-CLIENT-PREFACE for server, INITIALIZE-INSTANCE for client)
or from the @CALLBACKS and each calls WRITE-FRAME that calls in turn
WRITE-FRAME-HEADER-TO-VECTOR to write the header itself. You should not need to
call these, but they are good ones to trace to debug low level problems. Each
write function takes object identifying the http stream or connection that the
frame affects, additional parameters, and optional parameters that usually
relate to the known flags."
  (write-data-frame function)
  (write-headers-frame function)
  (priority type)
  (write-priority-frame function)
  (write-rst-stream-frame function)
  (write-settings-frame function)
  (write-ack-setting-frame function)
  (write-push-promise-frame function)
  (write-ping-frame function)
  (write-goaway-frame function)
  (write-window-update-frame function)
  (write-continuation-frame function)
  (write-altsvc-frame function)
  (write-frame-header-to-vector function))

(defsection @frames-implementation
    (:title "Sending and receiving frames"
     :export nil)

  (frame-type class)
  (*frame-types* variable))


(defstruct (frame-type
            (:print-object (lambda (type stream)
                             (format stream "<~a>" (frame-type-name type)))))
  "Description of a frame type.

Apart from name and documentation, each frame type keeps this:
- RECEIVE-FN :: How to handle reading of received frames
- NEW-STREAM-STATE :: whether it can be used to create new streams (i.e., is allowed on streams in
  IDLE state) and if so what is new state of such stream
- OLD-STREAM-OK :: in what stream states the frame can be received
- CONNECTION-OK :: whether the frame can have STREAM-ID equal to zero, that is, act on connections.
"
  (documentation nil :type (or null string))
  (name nil :type symbol)
  (receive-fn (constantly nil) :type function)
  old-stream-ok new-stream-state connection-ok
  (bad-state-error nil :type (or null (unsigned-byte 8)))
  flag-keywords)

(defconstant +known-frame-types-count+ 256
  "Frame types are indexed by an octet.")

(defvar *frame-types*
  (let ((res
          (make-array +known-frame-types-count+
                      :initial-contents
                      (loop for type from 0 to 255
                            collect
                            (make-frame-type
                             :name (intern (format nil "UNKNOWN-FRAME-~D" type))
                             :documentation
                             "Frames of an unknown or unsupported types."
                             :receive-fn  (lambda (connection http-stream length flags)
                                       (declare (ignore http-stream))
                                       (handle-undefined-frame type flags length)
                                       (let ((stream (get-network-stream connection)))
                                         (dotimes (i length) (read-byte stream)))))))))
    res)
  "Array of frame types. It is populated later with DEFINE-FRAME-TYPE-OLD.")

(defun read-padding (stream padding-size)
  "Read the padding from the stream if padding-size is not NIL.

Padding is used by some frame types.

A receiver is not obligated to verify padding but MAY treat non-zero padding as
a connection error (Section 5.4.1) of type PROTOCOL_ERROR. For now we ignore the
padding."
  (dotimes (i padding-size) (read-bytes stream 1)))

(defun possibly-padded-body (buffer fn padded pars)
  "Add payload and possibly padding to a BUFFER that already contains 9 octets of the header."
  (cond
    ((null padded) (apply fn buffer 9 pars))
    (t
     (setf (aref buffer 9) (length padded)) ; padding
     (apply fn buffer 10 pars)
     (replace buffer padded :start1 (- (length buffer) (length padded)))))
  buffer)

(defun change-state-on-write-end (http-stream)
  "Change state of the stream when STREAM-END is sent."
  (with-slots (state) http-stream
    (ecase state
      (open (setf state 'half-closed/local))
      (half-closed/remote (close-http2-stream http-stream)))))

(defun padded-length (length padding)
  "Length of the frame with added padding (incl. padding size)."
  (if padding (+ 1 length (length padding)) length))

(defun check-stream-state-ok (connection http-stream ok-states bad-state-error)
  "Throw BAD-STATE-ERROR when the stream state is not appropriate for the frame type.

Return the HTTP-STREAM."
  (unless (member (get-state http-stream) ok-states)
    (connection-error 'bad-stream-state  connection
                      :code bad-state-error
                      :actual (get-state http-stream)
                      :allowed ok-states))
  http-stream)

(defvar *flag-codes-keywords*
    '(:padded 8 :end-stream 1 :ack 1 :end-headers 4 :priority 32)
    "Property list of flag names and their values..

This makes use of the fact that same flag name has same index in all headers
where it is used.")

(defun flags-to-code (pars)
  (loop for (par val) on pars by #'cddr
        when val
          sum (getf *flag-codes-keywords* par)))

(defun get-flag (flags flag-name)
  (plusp (logand flags (getf *flag-codes-keywords* flag-name))))

(defun has-flag (flags flag-name allowed)
  (when (member flag-name allowed)
       (get-flag flags flag-name)))

(defclass rw-pipe (pipe-end-for-write pipe-end-for-read)
  ())

(defmacro with-padding-marks ((connection padded start end) &body body)
  (let ((length (gensym "LENGTH")))
    `(let* ((,length (length data))
            (,end (if ,padded (- ,length (aref data 0)) ,length))
            (,start (if ,padded 1 0)))
       (when (<= ,end 0)
         (http2:connection-error 'too-big-padding ,connection))
       ,@body
       (if padded
           (values #'read-padding-from-vector ,length)
           (values #'parse-frame-header 9)))))

(defmacro define-frame-type (type-code frame-type-name documentation (&rest parameters)
                             (&key (flags nil) length
                                must-have-stream-in may-have-connection
                                must-have-connection
                                new-stream-state
                                (bad-state-error +stream-closed+)
                                has-reserved)
                             writer
                             (&body reader))
  "Define:
- A frame type object that allows to read frame of given type,
- a constant named `+foo+` that translates to TYPE-CODE,
- a writer function WRITE-FOO that takes CONNECTION or HTTP-STREAM and possibly
  other PARAMETERS and FLAGSs and writes appropriate frame.

Each PARAMETER is a list of name, size in bits or type specifier and documentation."
  (declare ((unsigned-byte 8) type-code)
           (keyword frame-type-name))
  (let (key-parameters
        (writer-name (intern (format nil "WRITE-~a" frame-type-name)))
        (parser-name (intern (format nil "PARSE-~a" frame-type-name)))
        (http-connection-or-stream
          (cond (must-have-connection 'connection)
                (may-have-connection 'stream-or-connection)
                (t 'stream))))
    ;; Reserved is used rarely so no need to force always specifying it
    (when has-reserved (push '(reserved t) key-parameters))
    ;; Priority is both a flag to set and value to store if this flags is set.
    (when (member 'priority flags) (push '(priority (or null priority)) key-parameters))
    `(progn
       (defconstant ,(intern (format nil "+~a+" frame-type-name)) ,type-code)

       (defun ,writer-name (,http-connection-or-stream
                            ,@(mapcar 'first parameters)
                            &rest keys
                            &key
                              ,@(union (mapcar 'first key-parameters)
                                       flags))
         ,documentation
         (declare (ignore ,@(remove 'priority flags)))
         (let ((length ,length))
           (write-frame
            ,http-connection-or-stream
            length
            ,type-code
            keys
            ,writer
            ,@(mapcar 'car (append parameters key-parameters)))))
       (defun ,parser-name ,@(cdr reader))
       (setf (aref *frame-types* ,type-code)
             (make-frame-type :name ,frame-type-name
                              :documentation ,documentation
                              :receive-fn #',parser-name
                              :old-stream-ok ',must-have-stream-in
                              :new-stream-state ',new-stream-state
                              :connection-ok ,(or may-have-connection must-have-connection)
                              :bad-state-error ,bad-state-error
                              :flag-keywords
                              ',(mapcar (lambda (a) (intern (symbol-name a) :keyword))
                                        flags))))))

(defun write-frame (http-connection-or-stream length type-code keys
                    writer &rest pars)
  "Universal function to write a frame to a stream and account for possible stream
state change.

Queues using QUEUE-FRAME an octet vector with the frame, including
frame header (9 octets) and padding octets.

The payload is generated using WRITER object. The WRITER takes CONNECTION and
PARS as its parameters."
  (let* ((padded (getf keys :padded))
         (buffer (make-octet-buffer (+ length 9 (if padded (+ 1 (length padded)) 0)))))
    (write-frame-header-to-vector
     buffer 0 (padded-length length padded)
     type-code (flags-to-code keys)
     (get-stream-id http-connection-or-stream) nil)
    (when writer
      (possibly-padded-body buffer writer padded pars))
    (queue-frame (get-connection http-connection-or-stream) buffer)
    (when (getf keys :end-stream)
      (change-state-on-write-end http-connection-or-stream))
    buffer))

(defun create-new-local-stream (connection &optional pars)
  "Create new local stream of default class on CONNECTION. Additional PARS are
passed to the make-instance"
  (let ((stream (apply #'make-instance (get-stream-class connection)
                       :stream-id (get-id-to-use connection)
                       :peer-window-size (get-initial-peer-window-size connection)
                       :window-size (get-initial-window-size connection)
                       :connection connection
                       :state 'open
                       pars)))
    (incf (get-id-to-use connection) 2)
    (push stream (get-streams connection))
    stream))

(defun write-32-bits (stream value)
  (write-bytes stream 4 value))

(defun write-31-bits (vector start value flag)
  "Write 31 bits of VALUE to a VECTOR. Set first bit if FLAG is set."
  (declare (optimize speed))
  (declare (type stream-id value))
  (setf (aref/wide vector start 4) (logior value (if flag #x80000000 0))))

(defun write-stream-id (stream value reserved)
  "Write STREAM-ID to the binary stream"
  (write-31-bits stream 0 value reserved))

(defun write-frame-header-to-vector (vector start length type flags stream-id R)
  "Write a frame header to STREAM."
;;; All frames begin with a fixed 9-octet header followed by a variable-
;;; length payload.
;;;
;;; +begin_src artist
;;;  +-----------------------------------------------+
;;;  |                 Length (24)                   |
;;;  +---------------+---------------+---------------+
;;;  |   Type (8)    |   Flags (8)   |
;;;  +-+-------------+---------------+-------------------------------+
;;;  |R|                 Stream Identifier (31)                      |
;;;  +=+=============================================================+
;;;  |                   Frame Payload (0...)                      ...
;;;  +---------------------------------------------------------------+
;;; +end_src
  (declare (type (unsigned-byte 24) length)
           (type (unsigned-byte 8) type flags)
           (type (simple-array (unsigned-byte 8)) vector)
           (type stream-id stream-id))
  (setf (aref vector start) (ldb (byte 8 16) length))
  (setf (aref vector (incf start)) (ldb (byte 8 8) length))
  (setf (aref vector (incf start)) (ldb (byte 8 0) length))
  (setf (aref vector (incf start)) type)
  (setf (aref vector (incf start)) flags)
  (setf (aref vector (incf start))
        (logior (if R #x80 0) (ldb (byte 7 23) stream-id)))
  (setf (aref vector (incf start)) (ldb (byte 8 16) stream-id))
  (setf (aref vector (incf start))  (ldb (byte 8 8) stream-id))
  (setf (aref vector (incf start))  (ldb (byte 8 0) stream-id))
  vector)

(defun find-http-stream-by-id (connection id frame-type)
  "Find HTTP stream in the connection.

Returns either HTTP2-STREAM object (existing or new), CONNECTION or one of :IDLE
:CLOSED for yet or already nonexistent streams.

Also do some checks on the stream id based on the frame type."
  (declare (optimize speed (safety 1) (debug 0))
           (type frame-type frame-type))
  (with-slots (streams last-id-seen) connection
    (declare (type stream-id id last-id-seen))
    (let ((new-stream-state (frame-type-new-stream-state frame-type))
          (our-id (is-our-stream-id connection id)))
      (cond
        ((zerop id)
         (if (frame-type-connection-ok frame-type)
             connection
             (connection-error 'frame-type-needs-stream connection)))
        ((and (not our-id) (> id last-id-seen) new-stream-state)
         (peer-opens-http-stream-really-open connection id new-stream-state))
        ((and our-id (>= id (the stream-id (get-id-to-use connection))))
         (connection-error 'our-id-created-by-peer connection))
        ((and (not our-id) (> id last-id-seen))
         (connection-error 'bad-stream-state connection
                           :actual 'idle
                           :code +protocol-error+
                           :allowed '(not idle)
                           :stream-id id))
        ((frame-type-old-stream-ok frame-type)
         (check-stream-state-ok connection
                                (find-just-stream-by-id streams id)
                                (frame-type-old-stream-ok frame-type)
                                (frame-type-bad-state-error frame-type)))
        (t
         (connection-error 'frame-type-needs-connection connection
                           :frame-type frame-type))))))

(defun find-just-stream-by-id (streams id)
  "Find STREAM by ID in STREAMS, or :closed

The list of streams should already be sorted from high number to low number, so we could
stop as soon as we can see lower value. However, we assume the list needed to be searched is
pretty short so we do not care."
  (or (find id streams :test #'= :key #'get-stream-id) :closed))

(defun checked-length (length connection)
  (declare (ftype (function (t) (unsigned-byte 24)) get-max-frame-size))

  (when (> length (the (unsigned-byte 24) (get-max-frame-size connection)))
    ;; fixme: sometimes connection error.
    (connection-error 'too-big-frame connection
                      :frame-size length
                      :max-frame-size (get-max-frame-size connection)))
  length)

(defun checked-R-flag (R)
  (when (plusp R) (warn 'reserved-bit-set))
  R)

(defun parse-frame-header (connection header &optional (start 0) (end (length header)))
  "Parse header (9 octets array) and return two values, a function to parse
following data (that can be again #'PARSE-FRAME-HEADER if there is no payload),
and size of data that the following function expects."
;;;  +-----------------------------------------------+
;;;  |                 Length (24)                   |
;;;  +---------------+---------------+---------------+
;;;  |   Type (8)    |   Flags (8)   |
;;;  +-+-------------+---------------+-------------------------------+
;;;  |R|                 Stream Identifier (31)                      |
;;;  +=+=============================================================+
;;;  |                   Frame Payload (0...)                      ...
;;;  +---------------------------------------------------------------+

  (declare
   (optimize speed)
   ((simple-array (unsigned-byte 8) *) header)
   ((integer 0 #.array-dimension-limit) start end))
  ;; first flush anything we should have send to prevent both sides waiting
  (assert (= 9 (- end start)))
  (let* ((length (aref/wide header start 3))
         (type (aref header (+ start 3)))
         (flags (aref header (+ start 4)))
         (http-stream+R (aref/wide header (+ start 5) 4))
         (http-stream (ldb (byte 31 0) http-stream+R))
         (R (ldb (byte 1 31) http-stream+R)))
    (declare ((unsigned-byte 24) length)
             ((unsigned-byte 8) type flags)
             (stream-id http-stream)
             (ftype (function (t) (unsigned-byte 24)) get-max-frame-size))

    ;; FIXME:
    ;; - for most frame types, read full data then
    ;; - for data and maybe headers frame read as it goes
    (when (> length (the (unsigned-byte 24) (get-max-frame-size connection)))
      ;; fixme: sometimes connection error.
      (connection-error 'too-big-frame connection
                        :frame-size length
                        :max-frame-size (get-max-frame-size connection)))
    (if (plusp R) (warn 'reserved-bit-set))
    (let* ((frame-type-object (aref (the simple-vector *frame-types*) type))
           (stream-or-connection
             (find-http-stream-by-id connection http-stream frame-type-object))
           (flag-keywords (frame-type-flag-keywords frame-type-object))
           ;; 20240708 TODO: Delegate padding to resepctive fn, same end-of-stream
           (padded (has-flag flags :padded flag-keywords)))
      (if (zerop length)
          (values #'parse-frame-header 9)
          (values (lambda (connection data)
                    (funcall (frame-type-receive-fn frame-type-object)
                             connection data padded
                             stream-or-connection flags
                             (has-flag flags :end-stream flag-keywords)))
                  length)))))

(defun maybe-end-stream (has-end-flag stream)
    (when has-end-flag
      (ecase (get-state stream)
        (half-closed/local (close-http2-stream stream))
        (open (setf (get-state stream) 'half-closed/remote)))
      (peer-ends-http-stream stream)))

(defun read-padding-from-vector (connection data)
  "Ignore the padding octets. Frame header is next
FIXME: might be also continuation-frame-header"
  (declare (ignorable data connection))
  (values #'parse-frame-header 9))

;;;; Definition of individual frame types.
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

    (lambda (connection data padded active-stream flags end-of-stream)
      "Read octet vectors from the stream and call APPLY-DATA-FRAME on them.

Reduce tracked incoming window.

Run PEER-ENDS-HTTP-STREAM callback on the stream if appropriate."
      (declare (ignorable flags))
      (with-padding-marks (connection padded start end)
        (account-read-window-contribution connection active-stream (- end start))
        (apply-data-frame active-stream data start end)
        (maybe-end-stream end-of-stream active-stream))))

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
    (lambda (buffer start headers priority)
      (when (cdr headers)
        (error "Multiple header groups not supported now."))
      (if priority ;; this is deprecated as in RFC9113 but still implemented
          (write-priority priority buffer start headers)
          ;; TODO: what is allowed headers format?
          (replace buffer (car headers) :start1 start)))

    ;; reader
    (lambda (connection data padded active-stream flags end-of-stream)
      "Read incoming headers and call ADD-HEADER callback for each header.

Call PROCESS-END-HEADERS and PEER-ENDS-HTTP-STREAM (in this order) if relevant
flag is set.

At the beginning, invoke APPLY-STREAM-PRIORITY if priority was present."
      (with-padding-marks (connection padded start end)
        (when (get-flag flags :priority)
          (read-priority data active-stream start)
          (incf start 5))
        (read-and-add-headers data active-stream start end (get-flag flags :end-headers))
        (if (get-flag flags :end-headers)
            ;; If the END_HEADERS bit is not set, this frame MUST be followed by
            ;; another CONTINUATION frame.
            (process-end-headers connection active-stream))
        (http2::maybe-end-stream end-of-stream active-stream))))

(defun read-and-add-headers (data http-stream start end end-headers)
  "Read http headers from payload in DATA, starting at START.

Returns next function to call and size of expected data. If the END-HEADERS was
present, it would be PARSE-FRAME-HEADER, if not a continuation frame is to be
read."
  ;; FIXME: add handler for failure to read full header.
  (loop
    with http-stream-id = (get-stream-id http-stream)
    with connection = (get-connection http-stream)
    with length = (- end start)
    ;; 20240708 TODO: stream -> octets
    with data-as-stream = (make-instance 'pipe-end-for-read :buffer data :index start
                                                            :end end)
    for (name value) =
                      (read-http-header data-as-stream
                                        (get-decompression-context connection))
    when name
      do (add-header connection http-stream name value)
    while (< (get-index data-as-stream) length))
  (values (if end-headers #'parse-frame-header #'read-continuation-frame-on-demand)
          9))

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
    (lambda (connection data padded http-stream flags end-of-stream)
      "Read priority frame. Invoke APPLY-STREAM-PRIORITY if priority was present."
      (declare (ignore connection flags padded end-of-stream))
      (unless (= 5 (length data))
        ;;   A PRIORITY frame with a length other than 5 octets MUST be treated as
        ;;   a stream error (Section 5.4.2) of type FRAME_SIZE_ERROR.
        (http-stream-error 'frame-size-error http-stream))
      (read-priority data http-stream 0)))

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
    (lambda (connection data padded http-stream flags end-of-stream)
      (declare (ignore padded end-of-stream))
      "Invoke PEER-RESETS-STREAM callback."
      (assert (zerop flags))
      (unless (= (length data) 4)
        (connection-error 'incorrect-rst-frame-size connection))
      (peer-resets-stream http-stream (aref/wide data 0 4))))


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
    (lambda (connection data padded http-stream flags end-of-stream)
      (declare (ignore http-stream padded end-of-stream))
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


(define-frame-type 5 :push-promise-frame
    "
   The PUSH_PROMISE frame (type=0x5) is used to notify the peer endpoint
   in advance of streams the sender intends to initiate.  The
   PUSH_PROMISE frame includes the unsigned 31-bit identifier of the
   stream the endpoint plans to create along with a set of headers that
   provide additional context for the stream.  Section 8.2 contains a
   thorough description of the use of PUSH_PROMISE frames.

   ```
    +-+-------------+-----------------------------------------------+
    |R|                  Promised Stream ID (31)                    |
    +-+-----------------------------+-------------------------------+
    |                   Header Block Fragment (*)                 ...
    +---------------------------------------------------------------+
   ```

   The PUSH_PROMISE frame payload has the following fields:

   R: A single reserved bit.

   Promised Stream ID:  An unsigned 31-bit integer that identifies the
      stream that is reserved by the PUSH_PROMISE.  The promised stream
      identifier MUST be a valid choice for the next stream sent by the
      sender (see \"new stream identifier\" in Section 5.1.1).

   Header Block Fragment:  A header block fragment (Section 4.3)
      containing request header fields.

   The PUSH_PROMISE frame defines the following flags:

   END_HEADERS (0x4):  When set, bit 2 indicates that this frame
      contains an entire header block (Section 4.3) and is not followed
      by any CONTINUATION frames.

      A PUSH_PROMISE frame without the END_HEADERS flag set MUST be
      followed by a CONTINUATION frame for the same stream.  A receiver
      MUST treat the receipt of any other type of frame or a frame on a
      different stream as a connection error (Section 5.4.1) of type
      PROTOCOL_ERROR."
    ((headers list)
     (promised-stream-id 31))
    (:length (+ 4 (reduce '+ (mapcar 'length headers)))
     :flags (padded end-headers)

     ;;    PUSH_PROMISE frames MUST only be sent on a peer-initiated stream that
     ;;    is in either the "open" or "half-closed (remote)" state.  The stream
     ;;    identifier of a PUSH_PROMISE frame indicates the stream it is
     ;;    associated with.  If the stream identifier field specifies the value
     ;;    0x0, a recipient MUST respond with a connection error (Section 5.4.1)
     ;;    of type PROTOCOL_ERROR.
     :must-have-stream-in (open half-closed/local)
     :bad-state-error +protocol-error+
     :has-reserved t)
    ;;writer
    (lambda (buffer start headers promised-stream-id reserved)
      (write-31-bits buffer start promised-stream-id reserved)
      (replace buffer (car headers) :start1 (+ start 4)))
    ;; reader
    (lambda (connection data padded http-stream flags end-of-stream)
      ;; TODO: fix reader to vector, implement, dont forget padding
      (declare (ignore connection data padded http-stream flags end-of-stream))
      "Raise an error, as we do not handle promise frames, and do not advertise that we
do."
      (error "Reading promise N/A")))


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
    (lambda (buffer start opaque-data)
      (setf (aref/wide buffer start 8) opaque-data)
      buffer)
    (lambda (connection data padded http-stream flags end-of-stream)
      (declare (ignore http-stream padded end-of-stream))
      "Invoke DO-PING unless the ping has ACK flag - in that case invoke DO-PONG"
      (unless (= (length data) 8)
        (connection-error 'incorrect-ping-frame-size connection))
      (let ((data (aref/wide data 0 8)))
        (if (get-flag flags :ack)
            (do-pong connection data)
            (do-ping connection data)))
      (values #'parse-frame-header 9)))

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
    (lambda (connection data padded http-stream flags end-of-stream)
      (declare (ignore http-stream padded end-of-stream))
      "Invoke DO-GOAWAY callback."
      (unless (zerop flags) (warn "Flags set for goaway frame: ~d" flags))
      (let ((last-id (aref/wide data 0 4))
            (error-code (aref/wide data 4 4))
            (data (subseq data 8)))
        (do-goaway connection (get-error-name error-code) last-id data))))

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

    (lambda (connection data padded http-stream flags end-of-stream)
      (declare (ignore padded end-of-stream))
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


;;;; FIXME: this is completly wrong (continuation frame)
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
     :flags (end-headers))
    (lambda (buffer start headers)
      (when (cdr headers)
        (error "Multiple header groups not supported now."))
      (replace buffer (car headers) :start1 start))

    ;; reader
    ;; If we needed continuation frame, we would be in READ-BYTE*.
    ;; TODO: Confirm and verify, otherwise require state CONTINUATION-NEEDED.
    (lambda (connection data padded http-stream flags end-of-stream)
      "Raise condition CONNECTION-ERROR. The continuation frame should be read only when
expected, and then it is parsed by a different function."
      (declare (ignore data padded http-stream flags end-of-stream))
      (connection-error 'unexpected-continuation-frame connection)))

(defun read-continuation-frame-on-demand (stream http2-stream-id)
  "Read continuation frame header when it is expected on STREAM.

Return two values, length of the payload and END-HEADERS flag."
  (let ((length (read-bytes stream 3))
        (type (read-byte stream))
        (flags (read-byte stream))
        (stream-identifier (ldb (byte 31 0) (read-bytes stream 4))))
    (unless (= type +continuation-frame+)
      (error "Expected continuation frame and got ~a, should handle this" type))
    (unless (= http2-stream-id stream-identifier)
      (error "Expected continuation for ~d, got ~d" http2-stream-id
             stream-identifier))
    (values length (get-flag flags :end-headers))))

(define-frame-type 10 :altsvc-frame
    "See RFC 7838.  The ALTSVC HTTP/2 frame advertises the availability of an
   alternative service to an HTTP/2 client.

```
    +-------------------------------+-------------------------------+
    |         Origin-Len (16)       | Origin? (*)                 ...
    +-------------------------------+-------------------------------+
    |                   Alt-Svc-Field-Value (*)                   ...
    +---------------------------------------------------------------+
```"
    ((origin (or null string))
     (alt-svc-field-value string))
    (:length (+ 2 (length origin) (length alt-svc-field-value))
     :may-have-connection t)

    (lambda (buffer start origin alt-svc-field-value)
      (setf (aref/wide buffer start 2) (length origin))
      (when origin
        (replace buffer
                 (map '(vector unsigned-byte 8)
                      'char-code origin)
                 :start1 (+ start 2)))
      (replace buffer
               (map '(vector unsigned-byte 8)
                    'char-code alt-svc-field-value)
               :start1 (+ 2 start (length origin))))

    ;; reader
    (lambda (connection data padded http-stream flags end-of-stream)
      "Parse ALT-SVC frame and invoke HANDLE-ALT-SVC callback."
      (declare (ignore end-of-stream padded))
      (unless (zerop flags) (warn "Flags set for altsvc frame: ~d" flags))
      (let* ((origin-len (aref/wide data 0 2))
             (alt-svc-field-value (subseq data (+ 2 origin-len))))
        (cond
          ((and (eq connection http-stream)
                (plusp origin-len))
           (handle-alt-svc connection
                           (when (plusp origin-len)
                                      (subseq data 2 (+ 2 origin-len)))
                           alt-svc-field-value))
          ((plusp origin-len)
           "An ALTSVC frame on a stream other than stream 0 containing non-empty \"Origin\"
   information is invalid and MUST be ignored.")
          ((eq connection http-stream)
           "An ALTSVC frame on stream 0 with empty (length 0) \"Origin\" information is
   invalid and MUST be ignored.")
          (t (handle-alt-svc http-stream nil alt-svc-field-value))))))

(defsection @old-frame-functions
    (:title "Read frames from Common Lisp streams")
  "This was the entry point for the version one of the library.

Reading from Common Lisp streams has problems with not being able to poll, as
well as some others I forgot."
  (read-frame function)
  (write-frame-header function))

(defun write-sequences (stream headers)
  "Write a tree of sequences to stream."
  (etypecase headers
    (null nil)
    (vector (write-sequence headers stream))
    (cons (map nil (lambda (a) (write-sequences stream a)) headers))))

(defun read-frame (connection &optional (network-stream (get-network-stream connection)))
  "Read one frame related to the CONNECTION from STREAM. Flush outstanding data to
write, read the header and process it."
  (declare (inline make-octet-buffer)
           (stream-based-connection-mixin))
  (force-output network-stream)
  (let ((buffer (make-octet-buffer 9)))
    (declare (dynamic-extent buffer))
    (when (< (read-sequence buffer network-stream) 9)
      (error 'end-of-file :stream connection))
    (multiple-value-bind (receive-fn length)
        (parse-frame-header connection buffer)
      (declare (compiled-function receive-fn)
               ((unsigned-byte 24) length))
      (loop while (not (equal #'parse-frame-header receive-fn))
            do
               (let* ((frame-content (make-octet-buffer length))
                      (read (read-sequence frame-content network-stream)))
                 (when (< read length)
                   (error 'end-of-file :stream connection))
                 (multiple-value-setq (receive-fn length)
                   (funcall receive-fn connection frame-content))))
      (force-output network-stream))))

(defun write-frame-header (stream length type flags http-stream R)
  "Write a frame header to STREAM."
  (write-sequence
   (write-frame-header-to-vector
    (make-octet-buffer 9) 0 length type flags (get-stream-id http-stream) R)
   stream))
