(in-package http2/core)

(defclass gzip-decoding-mixin ()
  ((dstate :accessor get-dstate :initarg :dstate)))

;; FIXME: store dstate only if needed --- 20240607
(defmethod process-end-headers :after (connection (stream gzip-decoding-mixin))
  (setf (get-dstate stream) (chipz:make-dstate 'chipz:gzip)))

(defvar *zip-buffer-size* 4096)
(defmethod apply-data-frame :around ((stream gzip-decoding-mixin) payload start end)
  "When there is a `Content-Encoding: gzip` header, decompress the data and call next
APPLY-DATA-FRAME methods."
  ;; 20240607 TODO: Window update should go elsewhere

  (write-window-update-frame (get-connection stream) (- end start))
  (write-window-update-frame stream (- end start))
  ;; 20240607 TODO: test on dstate, not on header

  (if (equal (cdr (assoc "content-encoding" (get-headers stream) :test 'equal)) "gzip")
      (let* ((buffer (make-array *zip-buffer-size* :element-type '(unsigned-byte 8))))
        (loop
          (multiple-value-bind (consumed produced)
              (chipz:decompress buffer (get-dstate stream) payload :input-start start
                                                                   :input-end end)
            (when (and (zerop consumed) (zerop produced)) (return))
            (when (plusp produced)
              (call-next-method stream buffer 0 produced))
            (incf start consumed))))
      (call-next-method)))
