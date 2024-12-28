(in-package http2/core)

(defsection @lisp-stream-emulation
    (:title "Emulate Lisp stream over frames")
  (compile-payload-from-stream mgl-pax:macro)
  (write-binary-payload function)
  (make-transport-output-stream function)
  (text-collecting-stream class)
  (multi-part-data-stream class)
  (http-stream-to-vector function))

;;;; FIXME: document the role of this file and classes

(defun make-transport-output-stream (http2-stream charset gzip)
  "An OUTPUT-STREAM built atop HTTP2-STREAM with possible translating CHARSET to
octets and compression (if GZIP set)."
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

; TODO: 2024-12-27 How it works with GET-BODY and BODY-COLLECTING-MIXIN?
(defun http-stream-to-vector (http-stream)
  "HTTP-STREAM should be a TEXT-COLLECTING-STREAM.

HTTP-STREAM-TO-VECTOR then assembles the text from individual chunks."
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
  "Run BODY with STREAM-NAME bound to a stream named STREAM-NAME. Return octets
that represent text written to that stream given specified CHARSET, possibly
compressed.

```cl-transcript
(http2/core:compile-payload-from-stream (foo :utf8 nil) (princ \"Hello😃\" foo))
=> #(72 101 108 108 111 240 159 152 131)
```
"
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
  (:default-initargs :window-size-increment-callback nil)
  (:documentation
   "When peer sends window size increment frame, call specified callback function.

This works together with WRITE-BINARY-PAYLOAD to make sure that the payload is
fully written, eventually."))

(defmethod apply-window-size-increment :after ((object multi-part-data-stream) increment)
  (with-slots (window-size-increment-callback) object
    (when window-size-increment-callback
      (funcall window-size-increment-callback object))))

(defun write-binary-payload (connection stream payload &key (end-stream t))
  "Write binary PAYLOAD to the http2 STREAM.

The payload is written in chunks of peer frame size, and if the available window
is not big enough we stop writing and return, making sure that the writing
continues when window size increases."
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
