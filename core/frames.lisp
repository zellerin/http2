;;;; Copyright 2022-2025 by Tomáš Zellerin

(in-package :http2/core)

(defsection @frames-for-classes
    ()
  (handle-undefined-frame generic-function)
  (queue-frame generic-function)
  (stream-based-connection-mixin class)
  (write-buffer-connection-mixin class)
  (get-max-peer-frame-size generic-function)
  (frame-context class)
  (flush-http2-data generic-function))

(defclass frame-context ()
  ((max-frame-size           :accessor get-max-frame-size           :initarg :max-frame-size)
   (max-peer-frame-size      :accessor get-max-peer-frame-size      :initarg :max-peer-frame-size))
  (:default-initargs :max-frame-size 16384
                     :max-peer-frame-size 16384))

(defgeneric handle-undefined-frame (connection type flags data)
  (:method (connection type flags data)
    (warn 'unimplemented-feature :format-control "Frame type ~a is not supported" type))
  (:documentation
   "Callback that is called when a frame of unknown type is received."))

(defclass write-buffer-connection-mixin ()
  ((to-write :accessor get-to-write :initarg :to-write))
  (:default-initargs :to-write
   (make-array 3 :fill-pointer 0
                 :adjustable t))
  (:documentation
   "Stores queued frame in a per-connection write buffer. The internals of the
buffer are opaque."))


(defclass stream-based-connection-mixin ()
  ((network-stream :accessor get-network-stream :initarg :network-stream))
  (:documentation
   "A mixin for connections that read frames from and write to Common Lisp stream (in
slot NETWORK-STREAM)."))

(defgeneric flush-http2-data (connection)
  (:documentation "Send all the pending connection data to the peer.")
  (:method (connection)
    nil ; maybe nothing needed
    )
  (:method ((connection stream-based-connection-mixin))
    (handler-case
        (force-output (get-network-stream connection))
      (cl+ssl::ssl-error-syscall ()
        (error 'end-of-file :stream connection)))))

(defgeneric queue-frame (connection frame)
  (:documentation "Send or queue FRAME (octet vector) to the connection.

Each connection has to actually implement it.

Existing implementations are for:

- STREAM-BASED-CONNECTION-MIXIN, where the data are simply sent to the List stream,
- WRITE-BUFFER-CONNECTION-MIXIN, where the data are pushed to a buffer.
")

  (:method ((connection write-buffer-connection-mixin) frame)
    (with-slots (to-write) connection
      (let ((old-size (fill-pointer to-write)))
        (adjust-array to-write (+ (length to-write) (length frame)))
        (incf (fill-pointer to-write) (length frame))
        (replace to-write frame :start1 old-size)))
    frame)
  (:method ((connection stream-based-connection-mixin) frame)
    (write-sequence frame (get-network-stream connection))
    frame))

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

- set current frame parser to an initial one (e.g., PARSE-FRAME-HEADER or PARSE-CLIENT-PREFACE)  and expected length to expected (e.g., 9)
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



(deftype receiver-fn ()
  "Function that reads and processes number of octets, and returns next function and its needed size."
  `(function (http2-connection octet-vector &optional frame-size frame-size)
             ;; SBCL wants &optional to make clear no more values
             (values compiled-function frame-size &optional)))

(deftype frame-code-type ()
  `(unsigned-byte 8))

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
  (receive-fn (constantly nil) :type (and compiled-function (function (t t) receiver-fn)))
  old-stream-ok new-stream-state connection-ok
  (bad-state-error nil :type (or null (unsigned-byte 8)))
  flag-keywords)

(defconstant +allowed-frame-types-count+ 256
  "Frame types are indexed by an octet.")

(defun make-unknown-frame-type (type)
  (make-frame-type
   :name (intern (format nil "UNKNOWN-FRAME-~D" type))
   :old-stream-ok t :connection-ok t
   :documentation
   "Frames of an unknown or unsupported types."
   :receive-fn (let ((type type))
                 (lambda (stream-or-connection flags)
                   (declare (ignore stream-or-connection))
                   (lambda (connection data)
                     (handle-undefined-frame connection type flags data)
                     (values
                      #'parse-frame-header 9))))))

(defvar *frame-types*
  (map 'vector  #'make-unknown-frame-type (alexandria:iota +allowed-frame-types-count+))
  "Array of frame types. It is populated later with DEFINE-FRAME-TYPE.")

(deftype writer-fn ()
  "Function that write to an OCTET-VECTOR starting at FRAME-SIZE position."
  `(and compiled-function (function (octet-vector frame-size &rest t))))


(defsection @flags ()
  "HTTP/2 frames have a flags octet. We represent individual tags with a keyword and
provide
functions to convert between those two representation.

TODO: In future I might change representation to +ack+ being directly one and
skip translating completely."
  (flags-to-code function)
  (get-flag function))

(defvar *flag-codes-keywords*
  '(:padded 8 :end-stream 1 :ack 1 :end-headers 4 :priority 32)
  "Property list of tag names and their values..

This makes use of the fact that same flag name has same index in all headers
where it is used.")

(defun flags-to-code (pars)
  "Convert list of flags to a flag octet code. This is not supposed to be fast.

```cl-transcript
(http2/core:flags-to-code '(:padded t))
=> 8
```
"
  (loop for (par val) on pars by #'cddr
        when val
          sum (getf *flag-codes-keywords* par)))

(defun get-flag (flags flag-name)
  "Boolean that indicates whether the flag of FLAG-NAME is in FLAGS.

```cl-transcript
(get-flag 21 :ack)
=> T
```

For constant FLAG-NAME this is supposed to be fast.
"
  (declare ((unsigned-byte 8) flags)
           (keyword flag-name))
  (plusp (logand flags (getf *flag-codes-keywords* flag-name))))

(define-compiler-macro get-flag (&whole whole flags flag-name)
  (if (keywordp flag-name)
      `(plusp (logand ,flags ,(getf *flag-codes-keywords* flag-name)))
      whole))

#+unused
(defun has-flag (flags flag-name allowed)
  "Does the FLAGS octet contain tag FLAG-NAME "
  (declare ((unsigned-byte 8) flags)
           (keyword flag-name))
  (when (member flag-name allowed)
    (get-flag flags flag-name)))

(defsection @padding ()
  "Optional padding is represented by a NIL or an array of padding bytes."
  (padding type))

(deftype padding ()
  "If padding is present, it is an octet vector up to 256 bytes"
  `(or null octet-vector))

(defun padded-length (length padding)
  "Length of the frame with added padding (incl. padding size)."
  (declare (type padding padding))
  (if padding (+ 1 length (length padding)) length))

(defsection @padding-write ()
  )

(defun write-body-and-padding (buffer fn padded pars)
  "Add payload and possibly padding to a BUFFER that already contains 9 octets of the header."
  (declare (writer-fn fn)
           (octet-vector buffer)
           (padding padded))
  (cond
    ((null padded) (apply fn buffer 9 pars))
    (t
     (setf (aref buffer 9) (length padded)) ; padding
     (apply fn buffer 10 pars)
     (replace buffer padded :start1 (- (length buffer) (length padded)))))
  buffer)

(defsection @padding-read ()
  (with-padding-marks macro))

(defmacro with-padding-marks ((connection flags start end) &body body)
  "Look at the FLAGS and LENGTH (captured variable, FIXME) of a frame, with
possibly padded payload that starts at START, and adjust START and set END for
this frame to start and end of the actual payload.

CONNECTION is used only to have somewhere to signal possible error."
  ;; FIXME: does it work with START != 0? What is length then?
  `(let* ((padded (get-flag ,flags :padded))
          (,end length))
     (when padded
       (decf ,end (aref data ,start))
       (incf ,start))
     (when (< ,end ,start)
       (connection-error 'too-big-padding ,connection))
     ,@body))

(defsection @frame-definitions ())

(defmacro define-frame-writer (type-code writer-name http-connection-or-stream
                               flags length parameters key-parameters
                               documentation
                               body)
  "Define a function named WRITER-NAME name writes a frame of type TYPE-CODE"
  `(defun ,writer-name (,http-connection-or-stream
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
        ,body
        ,@(mapcar 'car (append parameters key-parameters))))))

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
  (declare (frame-code-type type-code)
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
       ,(when writer
          `(define-frame-writer ,type-code ,writer-name ,http-connection-or-stream
               ,flags ,length ,parameters ,key-parameters ,documentation ,writer))
       ,(when reader
          `(defun ,parser-name ,(cddr (second reader)) ; http-stream flags, or so
            (declare (ignorable ,@ (cddr (second reader))))
            (values (lambda (connection data &optional (start 0) (length (length data)))
                      ,@(cddr reader)))))
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
         (padded-length (padded-length length padded))
         (buffer (make-octet-buffer (+ 9 padded-length))))
    (write-frame-header-to-vector buffer 0 padded-length type-code (flags-to-code keys)
                                  (get-stream-id http-connection-or-stream) nil)
    (when writer
      (write-body-and-padding buffer writer padded pars))
    (queue-frame (get-connection http-connection-or-stream) buffer)
    (when (getf keys :end-stream)
      (change-state-on-write-end http-connection-or-stream))
    buffer))

(defun write-31-bits (vector start value flag)
  "Write 31 bits of VALUE to a VECTOR. Set first bit if FLAG is set."
  (declare (optimize speed))
  (declare (type stream-id value))
  (setf (aref/wide vector start 4) (logior value (if flag #x80000000 0))))

(defun write-stream-id (stream value reserved)
  "Write STREAM-ID to the binary stream"
  (write-31-bits stream 0 value reserved))

(defun write-frame-header-to-vector (vector start length type flags stream-id R)
  "Write a frame header to an octet buffer."
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
           (type (frame-code-type) type)
           (type (unsigned-byte 8) flags)
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

(defun checked-length (length connection)
  (declare (ftype (function (t) frame-size) get-max-frame-size))

  (when (> length (the frame-size (get-max-frame-size connection)))
    ;; fixme: sometimes connection error.
    (connection-error 'too-big-frame connection
                      :frame-size length
                      :max-frame-size (get-max-frame-size connection)))
  length)

(defun checked-R-flag (R)
  (when (plusp R) (warn 'reserved-bit-set))
  R)

(defun decode-frame-header (header start)
  "Decode frame header into several values:

- frame type object,
- length of the frame,
- flags,
- relevant stream, and
- R (reserved bit).

This function is primarily factored out to be TRACEd to see arriving frames."
  (declare (optimize speed)
           ((simple-array (unsigned-byte 8) *) header)
           (fixnum start)
           ((simple-array t *) *frame-types*))
  (let* ((length (aref/wide header start 3))
         (type (aref header (+ start 3)))
         (flags (aref header (+ start 4)))
         (http-stream+R (aref/wide header (+ start 5) 4))
         (http-stream (ldb (byte 31 0) http-stream+R))
         (R (ldb (byte 1 31) http-stream+R)))
    (values (aref *frame-types* type) length flags http-stream R)))

(declaim (ftype receiver-fn read-padding-from-vector parse-frame-header))

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
   (octet-vector header)
   (frame-size start end)
   (http2-connection connection))
  (assert (= 9 (- end start)))
  (multiple-value-bind (frame-type-object length flags http-stream R)
             (decode-frame-header header start)
           (declare (frame-size length)
                    (stream-id http-stream)
                    (bit R)
                    (ftype (function (t) (unsigned-byte 24)) get-max-frame-size))

           ;; FIXME:
           ;; - for most frame types, read full data then
           ;; - for data and maybe headers frame read as it goes

           (when (> length (the frame-size (get-max-frame-size connection)))
             ;; fixme: sometimes connection error.
             (connection-error 'too-big-frame connection
                               :frame-size length
                               :max-frame-size (get-max-frame-size connection)))
           (if (plusp R) (warn 'reserved-bit-set))
           (let* ((stream-or-connection
                    (find-http-stream-by-id connection http-stream frame-type-object)))
             (cond
               ((zerop length)
                ;; e.g., empty HEADER-FRAME still can have end-streams or end-headers flag
                (let ((next
                        (funcall (frame-type-receive-fn frame-type-object) stream-or-connection flags)))
                  (declare (receiver-fn next))
                  (funcall next connection (make-octet-buffer 0))))
               (t
                (values
                 (funcall (frame-type-receive-fn frame-type-object) stream-or-connection flags)
                 length))))))

(defun read-padding-from-vector (connection data &optional (start 0) (end 0))
  "Ignore the padding octets. Frame header is next
FIXME: might be also continuation-frame-header"
  (declare (ignorable data connection start end))
  (values #'parse-frame-header 9))

(defun get-flag-keywords (frame-type flags)
  (when (numberp frame-type)
    (setf frame-type (aref *frame-types* frame-type)))
  (loop for flag-name in (frame-type-flag-keywords frame-type)
        when (get-flag flags flag-name)
          collect flag-name))

#+sbcl
(defun trace-frames ()
  "Trace incoming and outgoing frames on a generic level. Example:
```
(http2/core::trace-frames)
"
  (trace-object decode-frame-header 0 (nil)
                ("Read ~A, size ~d~@[, flags ~{~a~^,~}~]~@[, stream ~a~]~@[ reserved bit set~]"
                 (& 0)  (& 1)  (get-flag-keywords (& 0) (& 2))
                 (when (plusp (sb-debug:arg 3)) (& 3)) (plusp (& 4))))
  (trace-object write-frame-header-to-vector 0
                ("Writing ~A, size ~d~@[, flags ~{~a~^,~}~]~@[, stream ~a~]~@[ reserved bit set~]"
                 (aref *frame-types* (& 3))  (& 4) (get-flag-keywords (& 3) (& 2))
                 (when (plusp (sb-debug:arg 5)) (& 5)) (& 6))))

(defun process-frames (connection data)
  "Process DATA as frames by CONNECTION."
  (loop
    with start of-type frame-size = 0 and end of-type frame-size = (length data)
    with fn of-type receiver-fn = #'parse-frame-header
    with size of-type frame-size = 9
    for old-size of-type frame-size = size
    while (> end start)
    do
       (multiple-value-setq (fn size) (funcall fn connection data start (min end (+ start size))))
       (incf start old-size)))
