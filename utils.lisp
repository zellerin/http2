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
        with i from 0 to (1- (length prefix)) by 2
        collect (parse-integer prefix :start i :end (+ i 2) :radix 16) into l
        finally (return (map 'simple-vector 'identity l))))
