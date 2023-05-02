(in-package http2)

(defclass constant-output-stream (trivial-gray-streams:fundamental-binary-output-stream binary-stream)
  ((output-buffer   :accessor get-output-buffer))
  (:default-initargs :to-write 0 :to-store 0)
  (:documentation
   "Binary stream that accepts new octets to the output-buffer"))

(defmethod initialize-instance :after ((stream constant-output-stream) &key &allow-other-keys)
  (setf (get-output-buffer stream)
        (make-array 1024 :element-type '(unsigned-byte 8)
                         :fill-pointer 0 :adjustable t)))

(defmethod trivial-gray-streams:stream-write-byte ((stream constant-output-stream) byte)
  (with-slots (output-buffer) stream
    (vector-push-extend byte output-buffer)))

(defmacro compile-payload-from-stream ((stream-name charset gzip) &body body)
  `(let* ((transport (make-instance 'constant-output-stream))
          (base transport))
     (when ,gzip
       (setf transport (gzip-stream:make-gzip-output-stream transport)))
     (awhen ,charset
       (setf transport
             (flexi-streams:make-flexi-stream
              transport
              :external-format ,charset)))
     (with-open-stream (,stream-name transport)
       ,@body
       (get-output-buffer base))))

(defun write-binary-payload (connection stream payload &key (end-stream t))
  "Write binary PAYLOAD to the http2 STREAM.

The payload is written in chunks of frame size, and if the available window is
not big enough we wait for confirmation (note that other things may happen
during waiting, such as receiving request for another data if we act as the
server)."
  (loop
    with total-length = (length payload)
    and sent = 0
    for allowed-window = (min (get-peer-window-size connection)
                              (get-peer-window-size stream))
    and frame-size = (get-max-peer-frame-size connection)
    while (and (>= (- total-length sent) frame-size))
    do (cond ((>= allowed-window frame-size)
              (write-data-frame stream (subseq payload sent (+ frame-size sent))
                                :end-stream nil)
              (incf sent frame-size))
             (t
              (read-frame connection)))
    finally (write-data-frame stream (subseq payload sent)
                              :end-stream end-stream)))
