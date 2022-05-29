(in-package http2)
(defun read-bytes (stream n)
  "Read N bytes from stream to an integer"
  (declare ((integer 1 8) n))
  (let ((res 0))
    (dotimes (i n)
      (setf (ldb (byte 8 (* 8 (- n 1 i))) res) (read-byte stream)))
    res))


(defun http2-error (&rest pars)
  (apply 'error pars))
