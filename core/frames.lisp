;;;; Copyright 2022, 2023 by Tomáš Zellerin

;;;; http2.lisp

(in-package :http2)

(mgl-pax:defsection @terms
    (:export nil
     :title "Glossary terms")
  (http2-client mgl-pax:glossary-term)
  (connection-term mgl-pax:glossary-term)
  (connection-error mgl-pax:glossary-term)
  (peer mgl-pax:glossary-term)
  (receiver mgl-pax:glossary-term)
  (sender mgl-pax:glossary-term)
  (server mgl-pax:glossary-term)
  (http2-stream mgl-pax:glossary-term)
  (http2-stream-error mgl-pax:glossary-term))

(mgl-pax:define-glossary-term  http2-client
    (:title "HTTP/2 client")
    "The endpoint that initiates an HTTP/2 connection.  Clients send HTTP
      requests and receive HTTP responses.")

(mgl-pax:define-glossary-term  connection-term (:title "HTTP/2 connection")
                               "A transport-layer connection between two endpoints.")

(mgl-pax:define-glossary-term connection-error (:title "Connection error")
                              "An error that affects the entire HTTP/2 connection.")

(mgl-pax:define-glossary-term connection-endpoint ()
"Either the client or server of the connection.")

(mgl-pax:define-glossary-term frame ()
  "The smallest unit of communication within an HTTP/2
      connection, consisting of a header and a variable-length sequence
      of octets structured according to the frame type.")

(mgl-pax:define-glossary-term peer ()  "An endpoint.  When discussing a particular endpoint, peer refers to the endpoint that is remote to the primary subject of discussion.")

(mgl-pax:define-glossary-term receiver () "An endpoint that is receiving frames.")

(mgl-pax:define-glossary-term sender () "An endpoint that is transmitting frames.")

(mgl-pax:define-glossary-term server () "The endpoint that accepts an HTTP/2 connection.  Servers
      receive HTTP requests and send HTTP responses.")

(mgl-pax:define-glossary-term http2-stream () "A bidirectional flow of frames within the HTTP/2 connection.")

(mgl-pax:define-glossary-term http2-stream-error () "An error on the individual HTTP/2 stream.")

(mgl-pax:defsection @frames-api
  (:title "Sending and receiving frames")
  "Lowest level interace deals with sending and receiving individual frames. For
each frame type there is an anonymous read function called by READ-FRAME based
on the type, and write function (WRITE-DATA-FRAME, ...) The only exception is
continuations frame that are not expected when read-frame is called - they are
processed as part of processing preceding header frame.

The write function are expected to be called individually and each calls
WRITE-FRAME-HEADER to send common parts; you should not need to call it, but
it is a good one to trace to debug low level problems. Each write function takes
object identifying the http stream or connection that the frame affects,
additional parameters, and optional parameters that usually relate to the known
flags."

  (read-frame function)
  (write-frame-header function)
  (write-ack-setting-frame function))

(mgl-pax:defsection @frames-implementation
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
  "Array of frame types. It is populated later with DEFINE-FRAME-TYPE.")

(defun read-padding (stream padding-size)
  "Read the padding from the stream if padding-size is not NIL.

Padding is used by some frame types.

A receiver is not obligated to verify padding but MAY treat non-zero padding as
a connection error (Section 5.4.1) of type PROTOCOL_ERROR. For now we ignore the
padding."
  (dotimes (i padding-size) (read-bytes stream 1)))

(defun possibly-padded-body (stream fn padded pars)
  "Add padding code to BODY if needed."
  (cond
    ((null padded) (apply fn stream pars))
    (t
     (write-bytes stream 1 (length padded))
     (apply fn stream pars)
     (write-sequence padded stream))))

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
        (writer-name (intern (format nil "WRITE-~a" frame-type-name))))
    ;; Reserved is used rarely so no need to force always specifying it
    (when has-reserved (push '(reserved t) key-parameters))
    ;; Priority is both a flag to set and value to store if this flags is set.
    (when (member 'priority flags) (push '(priority (or null priority)) key-parameters))
    `(progn
       (defconstant ,(intern (format nil "+~a+" frame-type-name)) ,type-code)

       (defun ,writer-name (http-connection-or-stream
                            ,@(mapcar 'first parameters)
                            &rest keys
                            &key
                              ,@(union (mapcar 'first key-parameters)
                                       flags))
         ,documentation
         (declare (ignore ,@(remove 'priority flags)))
         (let ((length ,length))
           (write-frame
            http-connection-or-stream
            length
            ,type-code
            keys
            ,writer
            ,@(mapcar 'car (append parameters key-parameters)))))
       (setf (aref *frame-types* ,type-code)
             (make-frame-type :name ,frame-type-name
                              :documentation ,documentation
                              :receive-fn #',reader
                              :old-stream-ok ',must-have-stream-in
                              :new-stream-state ',new-stream-state
                              :connection-ok ,(or may-have-connection must-have-connection)
                              :bad-state-error ,bad-state-error
                              :flag-keywords
                              ',(mapcar (lambda (a) (intern (symbol-name a) :keyword))
                                        flags))))))

(defun write-frame (http-connection-or-stream length type-code keys
                  writer &rest pars)
  "Universal function to write a frame to a stream and account for it."
  (let ((padded (getf keys :padded))
        (stream (get-network-stream http-connection-or-stream)))
    (write-frame-header stream
                        (padded-length length padded)
                        type-code
                        (flags-to-code keys)
                        http-connection-or-stream
                        nil)
    (possibly-padded-body stream
                          writer
                          padded
                          pars))
  (when (getf keys :end-stream)
    (change-state-on-write-end http-connection-or-stream)))

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

(defun write-31-bits (stream value flag)
  "Write 31 bits of VALUE to a STREAM. Set first bit if FLAG is set."
  (declare (optimize speed))
  (declare (type stream-id value))
  (write-32-bits stream (logior value (if flag #x80000000 0))))

(defun write-stream-id (stream value reserved)
  "Write STREAM-ID to the binary stream"
  (write-31-bits stream value reserved))

(defun write-frame-header (stream length type flags http-stream R)
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
;;;
;;; Length:  The length of the frame payload expressed as an unsigned
;;;    24-bit integer.  Values greater than 2^14 (16,384) MUST NOT be
;;;    sent unless the receiver has set a larger value for
;;;    SETTINGS_MAX_FRAME_SIZE.
;;;
;;;    The 9 octets of the frame header are not included in this value.
;;;
;;; Type:  The 8-bit type of the frame.  The frame type determines the
;;;    format and semantics of the frame.
;;;
;;; Flags:  An 8-bit field reserved for boolean flags specific to the
;;;    frame type.
;;;
;;;    Flags are assigned semantics specific to the indicated frame type.
;;;    Flags that have no defined semantics for a particular frame type
;;;    MUST be ignored and MUST be left unset (0x0) when sending.
;;;
;;; R: A reserved 1-bit field.  The semantics of this bit are undefined,
;;;    and the bit MUST remain unset (0x0) when sending and MUST be
;;;    ignored when receiving.
;;;
;;; Stream Identifier:  A stream identifier (see Section 5.1.1) expressed
;;;    as an unsigned 31-bit integer.  The value 0x0 is reserved for
;;;    frames that are associated with the connection as a whole as
;;;    opposed to an individual stream."
  (declare (type (unsigned-byte 24) length)
           (type (unsigned-byte 8) type flags)
           (optimize speed))
  (let ((http-stream-id (get-stream-id http-stream)))
    (declare (type stream-id http-stream-id))
    (write-bytes stream 3 length)
    (write-byte type stream)
    (write-byte flags stream)
    (write-stream-id stream http-stream-id R)))

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

(defun read-frame (connection &optional (stream (get-network-stream connection)))
    "Read one frame related to the CONNECTION from STREAM. Flush outstanding data to
write, read the header and process it."
  (declare (inline make-octet-buffer))
  (force-output stream)
  (let ((buffer (make-octet-buffer 9)))
    (declare (dynamic-extent buffer))
    (read-sequence buffer stream)
    (maybe-lock-for-write connection)
    (process-frame-header buffer connection stream)
    (maybe-unlock-for-write connection)))

(defun process-frame-header (header connection stream)
  "Read one frame related to the CONNECTION from STREAM. All frames begin with a
fixed 9-octet header followed by a variable-length payload. The function reads
and processes the header, and then dispatches to a frame type specific
handler that calls appropriate callbacks."
;;;  +-----------------------------------------------+
;;;  |                 Length (24)                   |
;;;  +---------------+---------------+---------------+
;;;  |   Type (8)    |   Flags (8)   |
;;;  +-+-------------+---------------+-------------------------------+
;;;  |R|                 Stream Identifier (31)                      |
;;;  +=+=============================================================+
;;;  |                   Frame Payload (0...)                      ...
;;;  +---------------------------------------------------------------+
;;;
;;; Length:  The length of the frame payload expressed as an unsigned
;;;    24-bit integer.  Values greater than 2^14 (16,384) MUST NOT be
;;;    sent unless the receiver has set a larger value for
;;;    SETTINGS_MAX_FRAME_SIZE.
;;;
;;;    The 9 octets of the frame header are not included in this value.
;;;
;;; Type:  The 8-bit type of the frame.  The frame type determines the
;;;    format and semantics of the frame.  Implementations MUST ignore
;;;    and discard any frame that has a type that is unknown.
;;;
;;; Flags:  An 8-bit field reserved for boolean flags specific to the
;;;    frame type.
;;;
;;;    Flags are assigned semantics specific to the indicated frame type.
;;;    Flags that have no defined semantics for a particular frame type
;;;    MUST be ignored and MUST be left unset (0x0) when sending.
;;;
;;; R: A reserved 1-bit field.  The semantics of this bit are undefined,
;;;    and the bit MUST remain unset (0x0) when sending and MUST be
;;;    ignored when receiving.
;;;
;;; Stream Identifier:  A stream identifier (see Section 5.1.1) expressed
;;;    as an unsigned 31-bit integer.  The value 0x0 is reserved for
;;;    frames that are associated with the connection as a whole as
;;;    opposed to an individual stream."

  (declare
   (optimize speed)
   ((simple-array (unsigned-byte 8) *) header))
  ;; first flush anything we should have send to prevent both sides waiting
  (let* ((length (aref/wide header 0 3))
         (type (aref header 3))
         (flags (aref header 4))
         (http-stream+R (aref/wide header 5 4))
         (http-stream (ldb (byte 31 0) http-stream+R))
         (R (ldb (byte 1 31) http-stream+R)))
    (declare ((unsigned-byte 24) length)
             ((unsigned-byte 8) type flags)
             (stream-id http-stream)
             (ftype (function (t) (unsigned-byte 24)) get-max-frame-size))

    ;; FIXME:
    ;; - for most frame types, read full data then
    ;; - for data and maybe headers frame read as it goes
    (unwind-protect
         (progn
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
                  (padded (has-flag flags :padded flag-keywords)))
             (let ((padding-size (when padded (read-byte stream))))
               (declare ((or null (unsigned-byte 8)) padding-size))
               (when (and padded (>= padding-size length))
                 (connection-error 'too-big-padding connection))
               (funcall (frame-type-receive-fn frame-type-object)
                        stream connection stream-or-connection
                        (if padded (- length 1 padding-size) length) flags)
               (when padded (read-padding stream padding-size)))

             (when (has-flag flags :end-stream flag-keywords)
               (ecase (get-state stream-or-connection)
                 (half-closed/local (close-http2-stream stream-or-connection))
                 (open (setf (get-state stream-or-connection) 'half-closed/remote)))
               (peer-ends-http-stream stream-or-connection))
             (values
              (frame-type-name frame-type-object)
              (or stream-or-connection http-stream)))))))

(defun write-sequences (stream headers)
  "Write a list of sequences to stream."
  (map nil (lambda (a) (write-sequence a stream)) headers))

;;;; Definition of individual frame types.
(define-frame-type 0 :data-frame
    "#+begin_src artist

  +---------------+-----------------------------------------------+
  |                            Data (*)                         ...
  +---------------------------------------------------------------+
  #+end_src

  DATA frames (type=0x0) convey arbitrary, variable-length sequences of
  octets associated with a stream.  One or more DATA frames are used,
  for instance, to carry HTTP request or response payloads."
    ((data (or cons vector null)))
    (:length (if (consp data) (reduce #'+ data :key #'length) (length data))
     :flags (padded end-stream)
     :must-have-stream-in (open half-closed/local))
    (lambda (stream data)
      (typecase data
        (null) ; just to close stream
        (cons (write-sequences stream data))
        (t (write-sequence data stream)))
      (account-write-window-contribution (get-connection http-connection-or-stream) http-connection-or-stream length))

    (lambda (stream connection http-stream length flags)
      "Read octet vectors from the stream and call APPLY-DATA-FRAME on them."
      (declare (ignore flags))
      (let* ((data (make-octet-buffer length)))
        (account-read-window-contribution connection http-stream length)
        (loop while (plusp length)
              do (decf length (read-sequence data stream))
                 (apply-data-frame http-stream data)))))

(defun account-read-window-contribution (connection stream length)
  ;; TODO: throw an error when this goes below zero
  (decf (get-window-size connection) length)
  (decf (get-window-size stream) length))

(defun account-write-window-contribution (connection stream length)
  (decf (get-peer-window-size connection) length)
  (decf (get-peer-window-size stream) length))



(defun write-priority (stream priority)
  (write-31-bits stream
                 (priority-stream-dependency priority)
                 (priority-exclusive priority))
  (write-byte (priority-weight priority) stream))

(define-frame-type 1 :headers-frame
    "#+begin_src artist

    +-+-------------+-----------------------------------------------+
    |E|                 Stream Dependency? (31)                     |
    +-+-------------+-----------------------------------------------+
    |  Weight? (8)  |
    +-+-------------+-----------------------------------------------+
    |                   Header Block Fragment (*)                 ...
    +---------------------------------------------------------------+
   #+end_src

   The HEADERS frame (type=0x1) is used to open a stream (Section 5.1),
   and additionally carries a header block fragment.  HEADERS frames can
   be sent on a stream in the \"idle\", \"reserved (local)\", \"open\", or
   \"half-closed (remote)\" state."
    ((headers list))                    ;  &key dependency weight
    (:length (+ (if priority 5 0) (reduce '+ (mapcar 'length headers)))
     :flags (padded end-stream end-headers
                    ;; PRIORITY is both flag and prio content  to write
                    priority)
     :must-have-stream-in (open idle reserved/remote half-closed/local)
     :new-stream-state open)

    ;; writer
    (lambda (stream headers priority)
      (when priority ;; this is deprecated as in RFC9113 but still implemented
        (write-priority stream priority))
      (write-sequences stream headers))

    ;; reader
    (lambda (stream connection http-stream length flags)
      (when (get-flag flags :priority)
        (decf length 5)
        (read-priority stream http-stream))
      (read-and-add-headers connection http-stream length (get-flag flags :end-headers))
      (if (get-flag flags :end-headers)
          ;; If the END_HEADERS bit is not set, this frame MUST be followed by
          ;; another CONTINUATION frame.
          (process-end-headers connection http-stream))))


(defun read-and-add-headers (connection http-stream length end-headers)
  (loop
    with http-stream-id = (get-stream-id http-stream)
    with *bytes-left* = length
    and  *when-no-bytes-left-fn* =
                                 (lambda (stream)
                                   (if end-headers
                                       (connection-error 'missing-header-octets connection))
                                   (multiple-value-setq (*bytes-left* end-headers)
                                     (read-continuation-frame-on-demand stream http-stream-id)))
    while (or (plusp *bytes-left*) (null end-headers))
    for (name value) = (read-http-header (get-network-stream connection)
                                         (get-decompression-context connection))
    when name
      do (add-header connection http-stream name value)))

#|
   Prioritization information in a HEADERS frame is logically equivalent
   to a separate PRIORITY frame, but inclusion in HEADERS avoids the
   potential for churn in stream prioritization when new streams are
   created.  Prioritization fields in HEADERS frames subsequent to the
   first on a stream reprioritize the stream (Section 5.3.3).

|#
(defun read-priority (stream http-stream)
  (let* ((e+strdep (read-bytes stream 4))
         (weight (read-byte stream))
         (exclusive (plusp (ldb (byte 1 31) e+strdep)))
         (stream-dependency (ldb (byte 31 0) e+strdep)))
    (apply-stream-priority http-stream exclusive weight stream-dependency)))

(define-frame-type 2 :priority-frame
    "
   The PRIORITY frame (type=0x2) specifies the sender-advised priority
   of a stream (Section 5.3).

   #+begin_src artist
    +-+-------------------------------------------------------------+
    |E|                  Stream Dependency (31)                     |
    +-+-------------+-----------------------------------------------+
    |   Weight (8)  |
    +-+-------------+
   #+end_src

   The payload of a PRIORITY frame contains the following fields:

   E: A single-bit flag indicating that the stream dependency is
      exclusive (see Section 5.3).

   Stream Dependency:  A 31-bit stream identifier for the stream that
      this stream depends on (see Section 5.3).

   Weight:  An unsigned 8-bit integer representing a priority weight for
      the stream (see Section 5.3).  Add one to the value to obtain a
      weight between 1 and 256."
    ((priority priority))
    (:length 5
     :must-have-stream-in (idle reserved/remote reserved/local open half-closed/local half-closed/remote closed)
     :new-stream-state idle)
    #'write-priority
    (lambda (stream connection http-stream length flags)
      (declare (ignore connection flags))
      (unless (= length 5)
        ;;   A PRIORITY frame with a length other than 5 octets MUST be treated as
        ;;   a stream error (Section 5.4.2) of type FRAME_SIZE_ERROR.
        (http-stream-error 'frame-size-error stream))
      (read-priority stream http-stream)))

(define-frame-type 3 :rst-stream-frame
    "The RST_STREAM frame (type=0x3) allows for immediate termination of a
   stream.  RST_STREAM is sent to request cancellation of a stream or to
   indicate that an error condition has occurred.

   #+begin_src artist
    +---------------------------------------------------------------+
    |                        Error Code (32)                        |
    +---------------------------------------------------------------+
   #+end_src

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
    #'write-32-bits
    (lambda (stream connection http-stream length flags)
      (assert (zerop flags))
      (unless (= length 4)
        (connection-error 'incorrect-rst-frame-size connection))
      (peer-resets-stream http-stream (read-bytes stream 4))))


(define-frame-type 4 :settings-frame
    "#+begin_src artist
    +-------------------------------+
    |       Identifier (16)         |
    +-------------------------------+-------------------------------+
    |                        Value (32)                             |
    +---------------------------------------------------------------+
   #+end_src

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
    (lambda (stream settings)
      (dolist (setting settings)
        #-ecl(declare ((cons (or (unsigned-byte 16) symbol) (unsigned-byte 32)) setting))
        (write-bytes stream 2
                     (if (numberp (car setting)) (car setting)
                         (find-setting-code (car setting))))
        (write-32-bits stream (cdr setting))))
    ;;reader
    (lambda (stream connection http-stream length flags)
      (declare (ignore http-stream))
      (cond
        ((get-flag flags :ack)
         (when (plusp length)
           (error "Ack settings frame must be empty. We should close connection."))
         (peer-acks-settings connection))
        (t
         (unless (zerop (mod length 6))
           (connection-error 'incorrect-settings-frame-size connection))
         (loop
           for i from length downto 1 by 6
           for identifier = (read-bytes stream 2)
           and value = (read-bytes stream 4)
           for name = (find-setting-by-id identifier)
           ;;    An endpoint that receives a SETTINGS frame with any unknown or
           ;;    unsupported identifier MUST ignore that setting.
           when name
             do (set-peer-setting connection name value)
           finally (peer-expects-settings-ack connection))))))

(defun write-ack-setting-frame (connection)
  "Write ACK settings frame.

   ACK (0x1):  When set, bit 0 indicates that this frame acknowledges
      receipt and application of the peer's SETTINGS frame.  When this
      bit is set, the payload of the SETTINGS frame MUST be empty.
      Receipt of a SETTINGS frame with the ACK flag set and a length
      field value other than 0 MUST be treated as a connection error
      (Section 5.4.1) of type FRAME_SIZE_ERROR.  For more information,
      see Section 6.5.3 (\"Settings Synchronization\")."
  (write-frame-header (get-network-stream connection)
                      0 +settings-frame+ 1 connection nil))


(define-frame-type 5 :push-promise-frame
    "
   The PUSH_PROMISE frame (type=0x5) is used to notify the peer endpoint
   in advance of streams the sender intends to initiate.  The
   PUSH_PROMISE frame includes the unsigned 31-bit identifier of the
   stream the endpoint plans to create along with a set of headers that
   provide additional context for the stream.  Section 8.2 contains a
   thorough description of the use of PUSH_PROMISE frames.

   #+begin_src artist
    +-+-------------+-----------------------------------------------+
    |R|                  Promised Stream ID (31)                    |
    +-+-----------------------------+-------------------------------+
    |                   Header Block Fragment (*)                 ...
    +---------------------------------------------------------------+
   #+end_src

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
    (lambda (stream headers promised-stream-id reserved)
      (write-stream-id stream  promised-stream-id reserved)
      (write-sequences stream headers))
    ;; reader
    (lambda (stream connection http-stream length flags)
      (declare (ignore stream connection http-stream length flags))
      (error "Reading promise N/A")))


(define-frame-type 6 :ping-frame
    "The PING frame (type=0x6) is a mechanism for measuring a minimal
   round-trip time from the sender, as well as determining whether an
   idle connection is still functional.  PING frames can be sent from
   any endpoint.

   #+begin_src artist
    +---------------------------------------------------------------+
    |                                                               |
    |                      Opaque Data (64)                         |
    |                                                               |
    +---------------------------------------------------------------+
   #+end_src

                      Figure 12: PING Payload Format

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
    (lambda (stream opaque-data)
      (write-bytes stream 8 opaque-data))
    (lambda (stream connection http-stream length flags)
      (declare (ignore http-stream))
      (unless (= length 8)
        (connection-error 'incorrect-ping-frame-size connection))
      (let ((data (read-bytes stream 8)))
        (if (get-flag flags :ack)
            (do-pong connection data)
            (do-ping connection data)))))

(define-frame-type 7 :goaway-frame
    "#+begin_src artist
    +-+-------------------------------------------------------------+
    |R|                  Last-Stream-ID (31)                        |
    +-+-------------------------------------------------------------+
    |                      Error Code (32)                          |
    +---------------------------------------------------------------+
    |                  Additional Debug Data (*)                    |
    +---------------------------------------------------------------+
   #+end_src

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

    (lambda (stream last-stream-id error-code debug-data reserved)
      (write-stream-id stream last-stream-id reserved)
     (write-bytes stream 4 error-code)
     (write-sequence debug-data stream))

    ;; reader
    (lambda (stream connection http-stream length flags)
      (declare (ignore http-stream))
      (unless (zerop flags) (warn "Flags set for goaway frame: ~d" flags))
      (let ((last-id (read-bytes stream 4))
            (error-code (read-bytes stream 4))
            (data (make-octet-buffer (- length 8))))
        (read-sequence data stream)
        (do-goaway connection (get-error-name error-code) last-id data))))

(define-frame-type 8 :window-update-frame
    "#+begin_src artist
    +-+-------------------------------------------------------------+
    |R|              Window Size Increment (31)                     |
    +-+-------------------------------------------------------------+
  #+end_src

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
    (lambda (stream window-size-increment reserved)
      (write-31-bits stream  window-size-increment reserved))

    (lambda (stream connection http-stream length flags)
      (unless (zerop flags)
        (warn "Flags for windows size set: ~x/~b" flags flags))
      (unless (= 4 length)
        (connection-error 'incorrect-window-update-frame-size connection))
      (let ((window-size-increment (read-bytes stream 4)))
        (when (plusp (ldb (byte 1 31) window-size-increment))
          (warn "Reserved bit in WINDOW-UPDATE-FRAME is set"))
        (cond
          ((plusp window-size-increment)
           (apply-window-size-increment http-stream (ldb (byte 31 0) window-size-increment)))
          ((eq connection http-stream)
           (connection-error 'null-connection-window-update connection))
          (t
           (http-stream-error 'null-stream-window-update http-stream))))))


(define-frame-type 9 :continuation-frame
    "#+begin_src artist

    +---------------------------------------------------------------+
    |                   Header Block Fragment (*)                 ...
    +---------------------------------------------------------------+
#+end_src

The CONTINUATION frame (type=0x9) is used to continue a sequence of header block
fragments (Section 4.3).  Any number of CONTINUATION frames can be sent, as long
as the preceding frame is on the same stream and is a HEADERS, PUSH_PROMISE, or
CONTINUATION frame without the END_HEADERS flag set."
    ((headers list))
    (:length (reduce '+ (mapcar 'length headers))
     :flags (end-headers))
    #'write-sequences

    ;; reader
    ;; If we needed continuation frame, we would be in READ-BYTE*.
    ;; TODO: Confirm and verify, otherwise require state CONTINUATION-NEEDED.
    (lambda (stream-name connection http-stream length flags)
      (declare (ignore stream-name http-stream length flags))
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

    #+begin_src artist
    +-------------------------------+-------------------------------+
    |         Origin-Len (16)       | Origin? (*)                 ...
    +-------------------------------+-------------------------------+
    |                   Alt-Svc-Field-Value (*)                   ...
    +---------------------------------------------------------------+
   #+end_src"
    ((origin (or null string))
     (alt-svc-field-value string))
    (:length (+ 2 (length origin) (length alt-svc-field-value)))

    (lambda (stream origin alt-svc-field-value)
      (write-bytes stream 2 (length origin))
      (when origin
        (write-sequence (map '(vector unsigned-byte 8)
                             'char-code origin)
                        stream))
      (write-sequence (map '(vector unsigned-byte 8)
                           'char-code alt-svc-field-value)
                      stream))

    ;; reader
    (lambda (stream connection http-stream length flags)
      (unless (zerop flags) (warn "Flags set for altsvc frame: ~d" flags))
      (let* ((origin-len (read-bytes stream 2))
             (origin (when (plusp origin-len)
                       (make-octet-buffer origin-len)))
             (alt-svc-field-value (make-octet-buffer (- length 2 origin-len))))
        (when (plusp origin-len)
          (read-sequence origin stream))
        (read-sequence alt-svc-field-value stream)
        (cond
          ((and (eq connection http-stream)
                (plusp origin-len))
           (handle-alt-svc connection origin alt-svc-field-value))
          ((plusp origin-len)
           "An ALTSVC frame on a stream other than stream 0 containing non-empty \"Origin\"
   information is invalid and MUST be ignored.")
          ((eq connection http-stream)
           "An ALTSVC frame on stream 0 with empty (length 0) \"Origin\" information is
   invalid and MUST be ignored.")
          (t (handle-alt-svc http-stream nil alt-svc-field-value))))))
