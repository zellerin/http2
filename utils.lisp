(in-package http2)

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

(defun http2-error (error-code &rest pars)
  (error "HTTP2 error ~a ~s" error-code pars))

(defvar *log-stream* (make-broadcast-stream)
  "Stream for logging output send by LOGGER.")

(defun logger (fmt &rest pars)
  "Send a format message to *LOG-STREAM*."
  (apply #'format *log-stream* fmt pars)
  (car pars))
