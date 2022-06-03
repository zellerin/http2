(in-package http2)

(defvar *bytes-read* nil "Number of bytes read from stream")

#|
The size of a frame payload is limited by the maximum size that a
receiver advertises in the SETTINGS_MAX_FRAME_SIZE setting.  This
setting can have any value between 2^14 (16,384) and 2^24-1
(16,777,215) octets, inclusive.
|#
(declaim ((or null (unsigned-byte 24)) *bytes-read*))

(defparameter *max-frame-size* 16384
  "Our frame size.

All implementations MUST be capable of receiving and minimally
processing frames up to 2^14 octets in length, plus the 9-octet frame
header (Section 4.1).  The size of the frame header is not included
when describing frame sizes.")

(declaim ((integer 16384 16777215) *max-frame-size*))

(defun read-byte* (stream)
  (incf *bytes-read*)
  (read-byte stream))

(defun read-bytes (stream n)
  "Read N bytes from stream to an integer"
  (declare ((integer 1 8) n))
  (let ((res 0))
    (dotimes (i n)
      (setf (ldb (byte 8 (* 8 (- n 1 i))) res) (read-byte stream)))
    res))

(defun write-bytes (stream n value)
  "write VALUE as N octets to  stream"
  (declare ((integer 1 8) n))
  (dotimes (i n)
    (write-byte (ldb (byte 8 (* 8 (- n 1 i))) value) stream)))

(defvar *log-stream* (make-broadcast-stream)
  "Stream for logging output send by LOGGER.")

(defun logger (fmt &rest pars)
  "Send a format message to *LOG-STREAM*."
  (apply #'format *log-stream* fmt pars)
  (car pars))

(defun vector-from-hex-text (text)
  ""
  (loop with prefix = text
        for i from 0 to (1- (length prefix)) by 2
        collect (parse-integer prefix :start i :end (+ i 2) :radix 16) into l
        finally (return (map 'simple-vector 'identity l))))

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
