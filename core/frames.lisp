;;;; Copyright 2022 by Tomáš Zellerin

;;;; http2.lisp

(in-package :http2)
#|
   client:  The endpoint that initiates an HTTP/2 connection.  Clients
      send HTTP requests and receive HTTP responses.

   connection:  A transport-layer connection between two endpoints.

   connection error:  An error that affects the entire HTTP/2
      connection.

   endpoint:  Either the client or server of the connection.

   frame:  The smallest unit of communication within an HTTP/2
      connection, consisting of a header and a variable-length sequence
      of octets structured according to the frame type.

   peer:  An endpoint.  When discussing a particular endpoint, "peer"
      refers to the endpoint that is remote to the primary subject of
      discussion.

   receiver:  An endpoint that is receiving frames.

   sender:  An endpoint that is transmitting frames.

   server:  The endpoint that accepts an HTTP/2 connection.  Servers
      receive HTTP requests and send HTTP responses.

   stream:  A bidirectional flow of frames within the HTTP/2 connection.

   stream error:  An error on the individual HTTP/2 stream.

|#


;;;; Frame definitions - boilerplate

;;;; For each frame type, we define a writing function (write-foo-frame) and a
;;;; reader function. The reader functions are stored in *frame-types* array so
;;;; that they can be dispatched after reading.

(defstruct (frame-type (:constructor make-frame-type
                           (name documentation receive-fn)))
  "Description of a frame type"
  name receive-fn documentation)

(defconstant +known-frame-types-count+ 256
  "Number of frame types we know.")

(defvar *frame-types*
  (let ((res
          (make-array 256)))
    (loop for type from 0 to 255
          do (setf (aref res type)
                   (make-frame-type
                    :unknown
                    "Implementations MUST discard frames that have unknown or unsupported types."
                    (lambda (connection http-stream length flags)
                      (declare (ignore http-stream))
                      (handle-undefined-frame type flags length)
                      (let ((stream (get-network-stream connection)))
                        (dotimes (i length) (read-byte stream)))))))
    res)
  "Array of frame types. It is populated later with DEFINE-FRAME-TYPE.")

(defun read-padding (stream padding-size)
  "Read the padding if padding-size is not NIL."
  ;; Padding octets MUST be set to zero when sending.  A receiver is
  ;; not obligated to verify padding but MAY treat non-zero padding as
  ;; a connection error (Section 5.4.1) of type PROTOCOL_ERROR.
  (when padding-size (dotimes (i padding-size) (read-bytes stream 1))))

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *flag-codes*
    '(padded 3 end-stream 0 ack 0 end-headers 2 priority 5)
    "Property list of flag names and their bit index.

This makes use of the fact that same flag name has same index in all headers
where it is used.")

  (defun flags-to-code (flags)
    "Create code to generate flag octet from FLAGS variable."
    (mapcar (lambda (a) `(if ,a ,(expt 2 (getf *flag-codes* a)) 0)) flags))

  (defun flags-to-vars-code (flags)
    "Create code to extract variables named as each member of *flag-codes*
that is set to T if it is in FLAGS and appropriate bit is set in the read flags."
    (loop for flag in *flag-codes* by 'cddr
          collect `(,flag ,(when (member flag flags)
                             `(plusp (ldb (byte 1 ,(getf *flag-codes* flag)) flags)))))))



(defmacro define-frame-type (type-code frame-type-name documentation (&rest parameters)
                             (&key (flags nil) length
                                must-have-stream-in must-not-have-stream
                                (bad-state-error +stream-closed+)
                                has-reserved)
                             writer-body
                             (&body reader))
  "This specification defines a number of frame types, each identified by a unique
8-bit TYPE CODE.  Each frame type serves a distinct purpose in the establishment
and management either of the connection as a whole or of individual streams.

The macro defining FRAME-TYPE-NAME :foo defines
- a constant named `+foo+` that translates to TYPE-CODE,
- a writer function WRITE-FOO that takes CONNECTION, HTTP-STREAM and possibly
  other PARAMETERS, sends frame header for given frame type with FLAGS and
  LENGTH expressions, and then executes WRITER-BODY with functions write-bytes
  and write-vector bound to writing to the transport stream. Each PARAMETER is
  a list of name, size in bits (or :variable) and documentation.
- a reader function named READ-FOO that takes CONNECTION, HTTP-STREAM, payload
  LENGTH and FLAGS and reads the payload of the frame: it runs READER-BODY with
  read-bytes and read-vector bound to reading from the transport stream. This
  function is supposed to be called from READ-FRAME that has already read the
  header and determined the appropriate http stream.

  The transmission of specific frame types can alter the state of a connection.
  If endpoints fail to maintain a synchronized view of the connection state,
  successful communication within the connection will no longer be possible.
  Therefore, it is important that endpoints have a shared comprehension of how
  the state is affected by the use any given frame."
  (declare ((unsigned-byte 8) type-code)
           (keyword frame-type-name))
  `(progn
     (defconstant ,(intern (format nil "+~a+" frame-type-name)) ,type-code)
     ,@(let ((reader-name (intern (format nil "READ-~a" frame-type-name)))
             (writer-name (intern (format nil "WRITE-~a" frame-type-name)))
             (stream-name (gensym "STREAM")))

;;; Writer
         `((defun ,writer-name (http-connection-or-stream
                                ,@ (mapcar 'first parameters)
                                &key ,@(when has-reserved '(reserved))
                                  ,@flags)
             ;; reserved parameter is a key and implicit
             ,@(when has-reserved (push '(reserved t) parameters)
                     nil)
             ,documentation
             (declare ,@(loop for par in parameters
                              collect `(,(if (integerp (second par))
                                             `(unsigned-byte ,(second par))
                                             (second par)
                                             )
                                        ,(first par))))
             (let ((,stream-name (get-network-stream http-connection-or-stream)))
               (flet ((write-bytes (n value) (write-bytes ,stream-name n value))
                      (write-vector (vector) (write-sequence vector ,stream-name))
                      (write-31 (value)
                        ,(if has-reserved
                             `(write-31-bits ,stream-name value reserved)
                             `(error "write-31 is not allowed here"))))
                 (declare (ignorable #'write-vector #'write-bytes #'write-vector
                                     #'write-31))
                 (write-frame-header ,stream-name
                                     (+ ,length
                                        ,@(when (find 'padded flags)
                                            `((if padded (1+ (length padded)) 0))))
                                     ,type-code
                                     (+ ,@(flags-to-code flags))
                                     http-connection-or-stream
                                     nil)
                 ,@(when (find 'padded flags)
                     `((when padded
                         (write-bytes 1 (length padded)))))
                 ,@writer-body
                 ,@(when (find 'padded flags)
                     '((when padded (write-vector padded))))
                 ,@(when (find 'end-stream flags)
                     '((when end-stream
                         (setf (get-state http-connection-or-stream)
                               (if (eql (get-state http-connection-or-stream) 'open)
                                   'half-closed/local 'closed))))))))

           (defun ,reader-name (connection http-stream length flags)
             ,documentation
             (let* ((,stream-name (get-network-stream connection))
                    ,@(flags-to-vars-code flags)
                    (padding-size (when padded (read-byte ,stream-name))))
               (declare (ignorable end-headers ack priority))
               (when padding-size (decf length (1+ padding-size)))
               (flet ((read-bytes (n) (read-bytes ,stream-name n))
                      (read-vector (seq) (read-sequence seq ,stream-name))
                      (protocol-error (text)
                        (http2-error connection 'protocol-error text)))
                 (declare (ignorable #'read-bytes #'read-vector #'protocol-error))
                 ,@(cond (must-have-stream-in
                          `((when (eq connection http-stream)
                              (protocol-error
                                           "Frame MUST be associated with a stream. If a
                              frame is received whose stream identifier field is
                              0x0, the recipient MUST respond with a connection
                              error (Section 5.4.1) of type PROTOCOL_ERROR."))
                            (unless (member (get-state http-stream)
                                            ',must-have-stream-in)
                              (http2-error connection ',bad-state-error "Frame can be applied only to streams in states ~{~A~^, ~}" ',must-have-stream-in))))
                         (must-not-have-stream
                          '((unless (eq connection http-stream)
                              ;; Frame MUST NOT be associated with a stream.  If
                              ;; a frame is received whose stream identifier
                              ;; field is 0x0, the recipient MUST respond with a
                              ;; connection error (Section 5.4.1) of type
                              ;; PROTOCOL_ERROR.
                              (protocol-error "Frame must not be associated with a stream")))))
                 ,@reader
                 (when end-stream
                   (setf (get-state http-stream)
                         (if (eql (get-state http-stream) 'open)
                             'half-closed/remote 'closed))
                   (peer-ends-http-stream http-stream)))
               (read-padding ,stream-name padding-size)))

           (setf (aref *frame-types* ,type-code)
                 (make-frame-type ,frame-type-name ,documentation
                                  #',reader-name))))))

(defun create-new-local-stream (connection &optional pars)
  "Create new local stream of default class on CONNECTION. Additional PARS are
passed to the make-instance"
  (let ((stream (apply #'make-instance (get-stream-class connection)
                       :stream-id (get-id-to-use connection)
                       :peer-window-size (get-initial-peer-window-size connection)
                       :window-size (get-initial-window-size connection)
                       :connection connection
                       :network-stream (get-network-stream connection)
                       pars)))
    (incf (get-id-to-use connection) 2)
    (push stream (get-streams connection))
    stream))

(defun write-31-bits (stream value reserved)
  "Write 31 bits of VALUE to a STREAM. Set first bit if RESERVED is set."
  (declare (optimize speed))
  (declare (type (unsigned-byte 31) value))
  (write-bytes stream 4 (logior value (if reserved #x80000000 0))))

(defun write-frame-header (stream length type flags http-stream R)
  "All frames begin with a fixed 9-octet header followed by a variable-
   length payload.

  #+begin_src artist
    +-----------------------------------------------+
    |                 Length (24)                   |
    +---------------+---------------+---------------+
    |   Type (8)    |   Flags (8)   |
    +-+-------------+---------------+-------------------------------+
    |R|                 Stream Identifier (31)                      |
    +=+=============================================================+
    |                   Frame Payload (0...)                      ...
    +---------------------------------------------------------------+
  #+end_src

   Length:  The length of the frame payload expressed as an unsigned
      24-bit integer.  Values greater than 2^14 (16,384) MUST NOT be
      sent unless the receiver has set a larger value for
      SETTINGS_MAX_FRAME_SIZE.

      The 9 octets of the frame header are not included in this value.

   Type:  The 8-bit type of the frame.  The frame type determines the
      format and semantics of the frame.

   Flags:  An 8-bit field reserved for boolean flags specific to the
      frame type.

      Flags are assigned semantics specific to the indicated frame type.
      Flags that have no defined semantics for a particular frame type
      MUST be ignored and MUST be left unset (0x0) when sending.

   R: A reserved 1-bit field.  The semantics of this bit are undefined,
      and the bit MUST remain unset (0x0) when sending and MUST be
      ignored when receiving.

   Stream Identifier:  A stream identifier (see Section 5.1.1) expressed
      as an unsigned 31-bit integer.  The value 0x0 is reserved for
      frames that are associated with the connection as a whole as
      opposed to an individual stream."
  (let ((http-stream-id (get-stream-id http-stream)))
    (declare (type (unsigned-byte 24) length)
             (type (unsigned-byte 8) type flags)
             (type (unsigned-byte 31) http-stream-id)
             (optimize speed))
    (write-bytes stream 3 length)
    (write-byte type stream)
    (write-byte flags stream)
    (write-31-bits stream http-stream-id R)))


(defun read-frame (connection &optional (stream (get-network-stream connection)))
  "All frames begin with a fixed 9-octet header followed by a variable-
   length payload.

  #+begin_src artist
    +-----------------------------------------------+
    |                 Length (24)                   |
    +---------------+---------------+---------------+
    |   Type (8)    |   Flags (8)   |
    +-+-------------+---------------+-------------------------------+
    |R|                 Stream Identifier (31)                      |
    +=+=============================================================+
    |                   Frame Payload (0...)                      ...
    +---------------------------------------------------------------+
  #+end_src

   Length:  The length of the frame payload expressed as an unsigned
      24-bit integer.  Values greater than 2^14 (16,384) MUST NOT be
      sent unless the receiver has set a larger value for
      SETTINGS_MAX_FRAME_SIZE.

      The 9 octets of the frame header are not included in this value.

   Type:  The 8-bit type of the frame.  The frame type determines the
      format and semantics of the frame.  Implementations MUST ignore
      and discard any frame that has a type that is unknown.

   Flags:  An 8-bit field reserved for boolean flags specific to the
      frame type.

      Flags are assigned semantics specific to the indicated frame type.
      Flags that have no defined semantics for a particular frame type
      MUST be ignored and MUST be left unset (0x0) when sending.

   R: A reserved 1-bit field.  The semantics of this bit are undefined,
      and the bit MUST remain unset (0x0) when sending and MUST be
      ignored when receiving.

   Stream Identifier:  A stream identifier (see Section 5.1.1) expressed
      as an unsigned 31-bit integer.  The value 0x0 is reserved for
      frames that are associated with the connection as a whole as
      opposed to an individual stream."
  ;; first flush anything we should have send to prevent both sides waiting
  (force-output stream)
  (let* ((length (read-bytes stream 3))
         (type (read-byte stream))
         (flags (read-byte stream))
         (http-stream+R (read-bytes stream 4))
         (http-stream (ldb (byte 31 0) http-stream+R))
         (R (ldb (byte 1 31) http-stream+R)))
    (declare ((unsigned-byte 24) length)
             ((unsigned-byte 8) type flags)
             ((unsigned-byte 31) http-stream))

    ;;  A frame
    ;; size error in a frame that could alter the state of the entire connection
    ;; MUST be treated as a connection error (Section 5.4.1); this includes any
    ;; frame carrying a header block (Section 4.3) (that is, HEADERS, PUSH_PROMISE,
    ;; and CONTINUATION), SETTINGS, and any frame with a stream identifier of 0.
    ;; Endpoints are not obligated to use all available space in a frame.
    ;; Responsiveness can be improved by using frames that are smaller than the
    ;; permitted maximum size.  Sending large frames can result in delays in sending
    ;; time-sensitive frames (such as RST_STREAM, WINDOW_UPDATE, or PRIORITY),
    ;; which, if blocked by the transmission of a large frame, could affect
    ;; performance.
    (new-frame-ready connection)
    (when (> length (get-max-frame-size connection))
        ;; fixme: sometimes connection error.
        (http2-error connection +frame-size-error+
                     "An endpoint MUST send an error code of FRAME_SIZE_ERROR if a frame exceeds the
size defined in SETTINGS_MAX_FRAME_SIZE, exceeds any limit defined for the frame
type, or is too small to contain mandatory frame data."))
    (if (plusp R) (warn "R is set, we should ignore it"))
    (let ((frame-type-object (aref *frame-types* type))
          (stream-or-connection
            (if (zerop http-stream) connection
                (find http-stream (get-streams connection)
                           :key #'get-stream-id))))
      (cond (frame-type-object
              (funcall (frame-type-receive-fn frame-type-object) connection
                       (if (zerop http-stream) connection
                           (or stream-or-connection
                               (peer-opens-http-stream connection http-stream type)))
                       length flags)))
      (values
       (frame-type-name frame-type-object)
       (or stream-or-connection http-stream)))))

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
    ((data (or cons vector)))
    (:length (if (consp data) (reduce #'+ data :key #'length) (length data))
     :flags (padded end-stream)
     :must-have-stream-in (open half-closed/local))
    ((if (consp data)
         (map nil #'write-vector data)
         (write-vector data)))

    ((when (minusp length)
        ;; 0 is ok, as there was one more byte in payload.
        (protocol-error "If the length of the padding is the length of the frame payload or greater, the
recipient MUST treat this as a connection error (Section 5.4.1) of type
PROTOCOL_ERROR"))
      (let* ((data (make-array length :element-type '(unsigned-byte 8))))
        (loop while (plusp length)
              do (decf length (read-vector data))
                 (apply-data-frame http-stream data)))))

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
    ((headers list)) ;  &key dependency weight
    (:length (+ (if priority 5 0) (reduce '+ (mapcar 'length headers)))
     :flags (padded end-stream end-headers
                    ;; PRIORITY: When set, bit 5 indicates that the Exclusive
                    ;; Flag (E), Stream Dependency, and Weight fields are
                    ;; present.
                    ;;
                    ;; Priority is a list (e+stream-dependency weight)
                    priority)
     :must-have-stream-in (idle reserved/remote open half-closed/local))

    ;; writer
    ((when priority ;; this is obsoleted, but still in place
       (write-bytes 4 (first priority))
       (write-bytes 1 (second priority)))
     (map nil #'write-vector headers))

    ;; reader
    ((when priority
       (decf length 5)
       (read-priority-frame connection http-stream 5 0))
      (when (minusp length)
        (protocol-error "Padding that exceeds the size remaining for the header block fragment MUST be treated as a PROTOCOL_ERROR."))
      (read-and-add-headers connection http-stream length end-headers)
      (if end-headers
        ;; If the END_HEADERS bit is not set, this frame MUST be followed by
        ;; another CONTINUATION frame.
          (process-end-headers connection http-stream))))

(defun read-and-add-headers (connection http-stream length end-headers)
  (let ((*bytes-left* length)
        (*when-no-bytes-left-fn* nil))
    (set-when-no-bytes-left-fn (get-stream-id http-stream) end-headers)
    (loop while (plusp *bytes-left*)
          for (name value) = (read-http-header (get-network-stream connection)
                                               (get-decompression-context connection))
          when name
          do (add-header connection http-stream name value))))

#|
   Prioritization information in a HEADERS frame is logically equivalent
   to a separate PRIORITY frame, but inclusion in HEADERS avoids the
   potential for churn in stream prioritization when new streams are
   created.  Prioritization fields in HEADERS frames subsequent to the
   first on a stream reprioritize the stream (Section 5.3.3).

|#
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
    ((exclusive t)
     (stream-dependency 31)
     (weight 8))
    (:length 5
     :must-have-stream-in (idle reserved/remote reserved/local open half-closed/local half-closed/remote closed))
    ((let ((e+strdep stream-dependency))
       (setf (ldb (byte 1 31) e+strdep) (if exclusive 1 0))
       (write-bytes 4 e+strdep)
       (write-bytes 1 weight)))
    ((unless (= length 5)
       ;;   A PRIORITY frame with a length other than 5 octets MUST be treated as
       ;;   a stream error (Section 5.4.2) of type FRAME_SIZE_ERROR.
       (http2-error connection 'frame-size-error
                    "A PRIORITY frame with a length other than 5 octets MUST be treated as a stream error (Section 5.4.2) of type FRAME_SIZE_ERROR.
"))
     (assert (= flags 0))
     (let* ((e+strdep (read-bytes 4))
            (weight (read-bytes 1))
            (exclusive (plusp (ldb (byte 1 31) e+strdep)))
            (stream-dependency (ldb (byte 31 0) e+strdep)))
       (apply-stream-priority http-stream exclusive weight stream-dependency))))

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
     :bad-state-error protocol_error)
    ((write-bytes 4 error-code))
    ((assert (zerop flags))
      (unless (= length 4)
        (http2-error connection 'frame-size-error
                     "A RST_STREAM frame with a length other than 4 octets MUST be treated as a connection error (Section 5.4.1) of type FRAME_SIZE_ERROR."))
      (peer-resets-stream http-stream (read-bytes 4))))


;;;; Settings
(defvar *settings-alist*
  '((:HEADER-TABLE-SIZE 1  "Allows the sender to inform the
      remote endpoint of the maximum size of the header compression
      table used to decode header blocks, in octets.  The encoder can
      select any size equal to or less than this value by using
      signaling specific to the header compression format inside a
      header block (see [COMPRESSION]).  The initial value is 4,096
      octets.")

    (:ENABLE-PUSH 2 "This setting can be used to disable
      server push (Section 8.2).  An endpoint MUST NOT send a
      PUSH-PROMISE frame if it receives this parameter set to a value of
      0.  An endpoint that has both set this parameter to 0 and had it
      acknowledged MUST treat the receipt of a PUSH-PROMISE frame as a
      connection error (Section 5.4.1) of type PROTOCOL-ERROR.

      The initial value is 1, which indicates that server push is
      permitted.  Any value other than 0 or 1 MUST be treated as a
      connection error (Section 5.4.1) of type PROTOCOL-ERROR.")

    (:MAX-CONCURRENT-STREAMS 3  "Indicates the maximum number
      of concurrent streams that the sender will allow.  This limit is
      directional: it applies to the number of streams that the sender
      permits the receiver to create.  Initially, there is no limit to
      this value.  It is recommended that this value be no smaller than
      100, so as to not unnecessarily limit parallelism.

      A value of 0 for SETTINGS-MAX-CONCURRENT-STREAMS SHOULD NOT be
      treated as special by endpoints.  A zero value does prevent the
      creation of new streams; however, this can also happen for any
      limit that is exhausted with active streams.  Servers SHOULD only
      set a zero value for short durations; if a server does not wish to
      accept requests, closing the connection is more appropriate.")

    (:INITIAL-WINDOW-SIZE 4  "Indicates the sender's initial
      window size (in octets) for stream-level flow control.  The
      initial value is 2^16-1 (65,535) octets.

      This setting affects the window size of all streams (see
      Section 6.9.2).

      Values above the maximum flow-control window size of 2^31-1 MUST
      be treated as a connection error (Section 5.4.1) of type
      FLOW-CONTROL-ERROR.")

    (:MAX-FRAME-SIZE 5  "Indicates the size of the largest
      frame payload that the sender is willing to receive, in octets.

      The initial value is 2^14 (16,384) octets.  The value advertised
      by an endpoint MUST be between this initial value and the maximum
      allowed frame size (2^24-1 or 16,777,215 octets), inclusive.
      Values outside this range MUST be treated as a connection error
      (Section 5.4.1) of type PROTOCOL-ERROR.")

    (:MAX-HEADER-LIST-SIZE 6  "This advisory setting informs a
      peer of the maximum size of header list that the sender is
      prepared to accept, in octets.  The value is based on the
      uncompressed size of header fields, including the length of the
      name and value in octets plus an overhead of 32 octets for each
      header field.

      For any given request, a lower limit than what is advertised MAY
      be enforced.  The initial value of this setting is unlimited.")

    (:enable-connect-protocol 8
     "See RFC8441. Upon receipt of SETTINGS_ENABLE_CONNECT_PROTOCOL with a value of
   1, a client MAY use the Extended CONNECT as defined in this document when
   creating new streams.  Receipt of this parameter by a server does not have
   any impact.")
    (:no-rfc5740-priorities 9 "See RFC9218.")
    (:tls-reneg-permitted #x10 "MS-HTTP2E"))
  "See https://www.iana.org/assignments/http2-parameters/http2-parameters.xhtml")

(defun find-setting-code (name)
  (or (second (assoc name *settings-alist*))
      (error "No setting named ~a" name)))

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
     :flags (ack)
     :must-not-have-stream t)
    ;; writer
    ((dolist (setting settings)
       #-ecl(declare ((cons (or (unsigned-byte 16) symbol) (unsigned-byte 32)) setting))
       (write-bytes 2 (if (numberp (car setting)) (car setting)
                          (find-setting-code (car setting))))
       (write-bytes 4 (cdr setting))))
    ;;reader
    ((cond
        (ack
         (when (plusp length)
           (error "Ack settings frame must be empty. We should close connection."))
         (peer-acks-settings connection))
        (t
         (unless (zerop (mod length 6))
           (http2-error connection +frame-size-error+
                        "A SETTINGS frame with a length other than a multiple of 6 octets MUST be treated as a connection error (Section 5.4.1) of type FRAME_SIZE_ERROR."))
         (loop
           for i from length downto 1 by 6
           for identifier = (read-bytes  2)
           and value = (read-bytes 4)
           for name = (find-setting-by-id identifier)
           ;;    An endpoint that receives a SETTINGS frame with any unknown or
           ;;    unsupported identifier MUST ignore that setting.
           when name
           do (set-peer-setting connection name value)
           finally (peer-expects-settings-ack connection))))))

(defun find-setting-by-id (id)
  (or (car (find id *settings-alist* :key 'second))
      (format nil "ID#~d" id)))

(defun write-ack-setting-frame (stream)
  "Write ACK settings frame.

   ACK (0x1):  When set, bit 0 indicates that this frame acknowledges
      receipt and application of the peer's SETTINGS frame.  When this
      bit is set, the payload of the SETTINGS frame MUST be empty.
      Receipt of a SETTINGS frame with the ACK flag set and a length
      field value other than 0 MUST be treated as a connection error
      (Section 5.4.1) of type FRAME_SIZE_ERROR.  For more information,
      see Section 6.5.3 (\"Settings Synchronization\")."
  (write-frame-header stream 0 +settings-frame+ 1 0 nil))


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
      PROTOCOL_ERROR.
"
    ((promised-stream-id 31)
     (headers list))
    (:length (+ 4 (reduce '+ (mapcar 'length headers)))
     :flags (padded end-headers)

     ;;    PUSH_PROMISE frames MUST only be sent on a peer-initiated stream that
     ;;    is in either the "open" or "half-closed (remote)" state.  The stream
     ;;    identifier of a PUSH_PROMISE frame indicates the stream it is
     ;;    associated with.  If the stream identifier field specifies the value
     ;;    0x0, a recipient MUST respond with a connection error (Section 5.4.1)
     ;;    of type PROTOCOL_ERROR.
     :must-have-stream-in (open half-closed/local)
     :bad-state-error protocol-error
     :has-reserved t)
    ;;writer
    ((write-31 promised-stream-id)
     (map nil #'write-vector headers))
    ;; reader
    ((error "Reading promise N/A")))


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
     :must-not-have-stream t)
    ;; writer
    ((write-bytes 8 opaque-data))
    ((unless (= length 8)
       (http2-error connection 'frame-size-error
                    "Receipt of a PING frame with a length field value other than 8 MUST be treated as a connection error (Section 5.4.1) of type FRAME_SIZE_ERROR."))
      (let ((data (read-bytes 8)))
        (if ack
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
     :must-not-have-stream t
     :has-reserved t)

    ((write-31 last-stream-id)
     (write-bytes 4 error-code)
     (write-vector debug-data))

    ;; reader
    ((unless (zerop flags) (warn "Flags set for goaway frame: ~d" flags))
      (let ((last-id (read-bytes 4))
            (error-code (read-bytes 4))
            (data (make-array (- length 8))))
        (read-vector data)
        (do-goaway connection (get-error-name error-code) last-id data))))

(define-frame-type 8 :window-update-frame
    "#+begin_src artist
    +-+-------------------------------------------------------------+
    |R|              Window Size Increment (31)                     |
    +-+-------------------------------------------------------------+
  #+end_src

The WINDOW_UPDATE frame (type=0x8) is used to implement flow control;  see Section 5.2 for an overview.  Flow control operates at two levels: on each individual stream and on the entire connection."
    ((window-size-increment 31))
    (:length 4
     :has-reserved t)
    ((write-31 window-size-increment))

    (;;reader
     (unless (zerop flags)
       (warn "Flags for windows size set: ~x/~b" flags flags))
      (unless (= 4 length) (error "A WINDOW_UPDATE frame with a length other than 4 octets MUST be  treated as a connection error (Section 5.4.1) of type FRAME_SIZE_ERROR."))
      (let ((window-size-increment (read-bytes 4)))
        (when (plusp (ldb (byte 1 31) window-size-increment))
          (warn "Reserved bit in WINDOW-UPDATE-FRAME is set"))
        (apply-window-size-increment http-stream (ldb (byte 31 0) window-size-increment)))))

(defun account-frame-window-contribution (connection stream length)
  (decf (get-window-size connection) length)
  (decf (get-window-size stream) length))

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
    ((map nil #'write-vector headers))

    ;; reader
    ;; If we needed continuation frame, we would be in READ-BYTE*.
    ((http2-error connection +protocol-error+
                  "A CONTINUATION frame MUST be preceded by a HEADERS, PUSH_PROMISE or
   CONTINUATION frame without the END_HEADERS flag set.  A recipient that
   observes violation of this rule MUST respond with a connection error (Section
   5.4.1) of type PROTOCOL_ERROR.")))

(defun set-when-no-bytes-left-fn (http2-stream-id end-headers)
  "Read a header of continuation frame if continuation expected. Set
*when-no-bytes-left-fn* appropriately."
  (setf *when-no-bytes-left-fn*
        (if end-headers
            (lambda (stream) (error "No bytes left in ~a, should raise malformed headers error"
                                    stream))
            (lambda (stream)
              (let ((length (read-bytes stream 3))
                    (type (read-byte stream))
                    (flags (read-byte stream))
                    (stream-identifier (ldb (byte 31 0) (read-bytes stream 4))))
                (unless (= type +continuation-frame+)
                  (error "Expected continuation frame and got ~a, should handle this" type))
                (unless (= http2-stream-id stream-identifier)
                  (error "Expected continuation for ~d, got ~d" http2-stream-id
                         stream-identifier))
                (set-when-no-bytes-left-fn http2-stream-id (plusp (logand flags #x4)))
                (setf *bytes-left* length))))))

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

    ((write-bytes 2 (length origin))
     (when origin (write-vector (map '(vector unsigned-byte 8)
                                     'char-code origin)))
     (write-vector (map '(vector unsigned-byte 8)
                        'char-code alt-svc-field-value)))

    ;; reader
    ((unless (zerop flags) (warn "Flags set for altsvc frame: ~d" flags))
      (let* ((origin-len (read-bytes 2))
             (origin (when (plusp origin-len)
                       (make-array origin-len :element-type '(unsigned-byte 8))))
             (alt-svc-field-value (make-array (- length 2 origin-len)
                                              :element-type '(unsigned-byte 8))))
        (when (plusp origin-len)
          (read-vector origin))
        (read-vector alt-svc-field-value)
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
