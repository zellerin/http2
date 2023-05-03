;;;; Copyright 2022, 2023 by Tomáš Zellerin

(in-package :http2)

(defvar *bytes-left* nil "Number of bytes left in frame")

(defvar *when-no-bytes-left-fn* nil "Function to call when no bytes are left. Either errors or calls continuations.")

(defvar *bytes-to-possibly-reuse* (make-array 0 :element-type '(unsigned-byte 8)
                                              :adjustable t
                                                :fill-pointer 0))
(defvar *bytes-to-reuse* nil)

#|
The size of a frame payload is limited by the maximum size that a
receiver advertises in the SETTINGS_MAX_FRAME_SIZE setting.  This
setting can have any value between 2^14 (16,384) and 2^24-1
(16,777,215) octets, inclusive.
|#
(declaim ((or null (unsigned-byte 24)) *bytes-read*))

(defun read-byte* (stream)
  (cond
    ((plusp *bytes-left*)
     (decf *bytes-left*)
     (read-byte stream))
    (t
     (funcall *when-no-bytes-left-fn* stream)
     (read-byte* stream))))

(defun read-bytes (stream n)
  "Read N bytes from stream to an integer"
  (declare ((integer 1 8) n))
  (let ((res 0))
    (dotimes (i n)
      (setf (ldb (byte 8 (* 8 (- n 1 i))) res) (read-byte stream)))
    res))

(defun write-bytes (stream n value)
  "write VALUE as N octets to stream. Maximum length is 64 bits (used by ping)."
  (declare (type (integer 1 8) n)
           (optimize speed)
           (type (unsigned-byte 64) value))
  (dotimes (i n)
    (write-byte (ldb (byte 8 (* 8 (- n 1 i))) value) stream)))

(defvar *log-stream* (make-broadcast-stream)
  "Stream for logging output send by LOGGER.")

(defun logger (fmt &rest pars)
  "Send a format message to *LOG-STREAM*."
  (apply #'format *log-stream* fmt pars)
  (car pars)
  (terpri *log-stream*))

(defun vector-from-hex-text (text)
  ""
  (loop with prefix = text
        for i from 0 to (1- (length prefix)) by 2
        collect (parse-integer prefix :start i :end (+ i 2) :radix 16) into l
        finally (return (map 'simple-vector 'identity l))))

;;;; Error codes
(defvar *error-codes*
  (macrolet ((defcode (name code documentation)
               `(progn
                  (defconstant ,name ,code ,documentation))))
    (vector
     (defcode +no-error+            0  "graceful shutdown")
     (defcode +protocol-error+      1  "protocol error detected")
     (defcode +internal-error+      2  "implementation fault")
     (defcode +flow-control-error+  3  "flow-control limits exceeded")
     (defcode +settings-timeout+    4  "settings not acknowledged")
     (defcode +stream-closed+       5  "frame received for closed stream")
     (defcode +frame-size-error+    6  "frame size incorrect")
     (defcode +refused-stream+      7  "stream not processed")
     (defcode +cancel+              8  "stream cancelled")
     (defcode +compression-error+   9  "compression state not updated")
     (defcode +connect-error+       #xa  "tcp connection error for connect method")
     (defcode +enhance-your-calm+   #xb  "processing capacity exceeded")
     (defcode +inadequate-security+ #xc  "negotiated tls parameters not acceptable")
     (defcode +http-1-1-required+   #xd  "Use HTTP/1.1 for the request")))

  "This table maps error codes to mnemonic names - symbols.

   Error codes are 32-bit fields that are used in RST_STREAM and GOAWAY
   frames to convey the reasons for the stream or connection error.

   Error codes share a common code space.  Some error codes apply only
   to either streams or the entire connection and have no defined
   semantics in the other context.")

(defun get-error-name (code)
  (if (<= 0 code #xd)
      (aref *error-codes* code)
      (intern (format nil "UNDEFINED-ERROR-CODE-~x" code) 'http2)))


;;;; Settings
(defvar *settings*
  #(nil
    (:HEADER-TABLE-SIZE 1  "Allows the sender to inform the
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

    (:accept-cache-digest 7              ; not part of standard
     "A server can notify its support for CACHE_DIGEST frame by sending the
      SETTINGS_ACCEPT_CACHE_DIGEST (0x7) SETTINGS parameter. If the server is
      tempted to making optimizations based on CACHE_DIGEST frames, it SHOULD
      send the SETTINGS parameter immediately after the connection is
      established.

      The value of the parameter is a bit-field of which the following bits are
      defined:

      ACCEPT (0x1): When set, it indicates that the server is willing to make
      use of a digest of cached responses.

      Rest of the bits MUST be ignored and MUST be left unset when sending.

      The initial value of the parameter is zero (0x0) meaning that the server
      is not interested in seeing a CACHE_DIGEST frame.

      Some underlying transports allow the server's first flight of application
      data to reach the client at around the same time when the client sends
      it's first flight data. When such transport (e.g., TLS 1.3
      [I-D.ietf-tls-tls13] in full-handshake mode) is used, a client can
      postpone sending the CACHE_DIGEST frame until it receives a
      SETTINGS_ACCEPT_CACHE_DIGEST settings value.

      When the underlying transport does not have such property (e.g., TLS 1.3
      in 0-RTT mode), a client can reuse the settings value found in previous
      connections to that origin [RFC6454] to make assumptions.")

    (:enable-connect-protocol 8
     "See RFC8441. Upon receipt of SETTINGS_ENABLE_CONNECT_PROTOCOL with a value of
   1, a client MAY use the Extended CONNECT as defined in this document when
   creating new streams.  Receipt of this parameter by a server does not have
   any impact.")
    (:no-rfc5740-priorities 9 "See RFC9218.")
    (:tls-reneg-permitted #x10 "MS-HTTP2E"))
  "See https://www.iana.org/assignments/http2-parameters/http2-parameters.xhtml")

(defun find-setting-code (name)
  "Find setting name by code"
  (or (position name *settings* :key #'car)
      (error "No setting named ~a" name)))

(defun find-setting-by-id (id)
  (if (< 0 id (length *settings*))
      (car (aref *settings* id))
      (format nil "ID#~d" id)))

(deftype stream-id ()
  "Streams are identified with an unsigned 31-bit  integer."
  `(unsigned-byte 31))

(deftype http2-stream-state ()
  "HTTP2 state. Currently a list, might be a number in future."
  '(member idle open closed
    half-closed/local half-closed/remote
    reserved/local reserved/remote))

(defstruct priority exclusive stream-dependency weight)
