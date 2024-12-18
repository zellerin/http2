(in-package http2/core)

(export 'compile-payload-from-stream)
(export 'write-binary-payload)
(export '(send-headers make-transport-output-stream text-collecting-stream
           multi-part-data-stream http-stream-to-vector))
;;;; FIXME: document the role of this file and classes

(defun make-transport-output-stream (http2-stream charset gzip)
  "An OUTPUT-STREAM built atop RAW STREAM with transformations based on HEADERS."
  (let* ((transport (make-instance 'http2/stream-overlay::payload-output-stream :base-http2-stream http2-stream)))
    (when gzip
      (setf transport (gzip-stream:make-gzip-output-stream transport)))
    (when charset
      (setf transport
            (flexi-streams:make-flexi-stream
             transport
             :external-format charset)))
    transport))

(defclass text-collecting-stream ()
  ((text :accessor get-text :initarg :text))
  (:default-initargs :text nil)
  (:documentation
   "Mixin that collect all the received body (possibly unzipped data frames
converted to proper encoding) into a TEXT slot."))

(defmethod http2/core::apply-text-data-frame ((stream text-collecting-stream) text)
  (push text (get-text stream)))

(defun http-stream-to-vector (http-stream)
  ;; 20240611 TODO: document
  (with-output-to-string (*standard-output*)
    (mapc 'princ (nreverse (get-text http-stream)))))

(defclass constant-output-stream (trivial-gray-streams:fundamental-binary-output-stream http2/stream-overlay::binary-stream)
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

(defclass multi-part-data-stream ()
  ((window-size-increment-callback :accessor get-window-size-increment-callback :initarg :window-size-increment-callback))
  (:default-initargs :window-size-increment-callback nil))

(defmethod apply-window-size-increment :after ((object multi-part-data-stream) increment)
  (with-slots (window-size-increment-callback) object
    (when window-size-increment-callback
      (funcall window-size-increment-callback object))))

(defun write-binary-payload (connection stream payload &key (end-stream t))
  "Write binary PAYLOAD to the http2 STREAM.

The payload is written in chunks of peer frame size, and if the available window
is not big enough we wait for confirmation (note that other things may happen
during waiting, such as receiving request for another data if we act as the
server)."
  (let ((sent 0)
        (total-length (length payload)))
    (with-slots (window-size-increment-callback) stream
      (flet ((write-chunk (stream)
               (loop
                 for allowed-window = (min (get-peer-window-size connection)
                                           (get-peer-window-size stream))
                 and frame-size = (get-max-peer-frame-size connection)
                 and data-to-send = (- total-length sent)
                 for have-full-frame-to-send = (>= data-to-send frame-size)
                 and allowed-send-full-frame = (>= allowed-window frame-size)
                 while (and have-full-frame-to-send allowed-send-full-frame)
                 do
                    (write-data-frame stream (subseq payload sent (+ frame-size sent))
                                      :end-stream nil)
                    (incf sent frame-size)
                 finally (when (>= allowed-window data-to-send)
                           (write-data-frame stream (subseq payload sent)
                                             :end-stream end-stream)
                           (setf window-size-increment-callback nil)))))
#+nil        (when window-size-increment-callback
          (error "FIXME: this is unsupported, do we really need :END-STREAM nil version?"))
        (setf window-size-increment-callback #'write-chunk)
        (write-chunk stream)))))
