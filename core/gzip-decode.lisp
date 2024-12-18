(in-package http2/core)

(defclass gzip-decoding-mixin ()
  ((dstate :accessor get-dstate :initarg :dstate)))

;; FIXME: store dstate only if needed --- 20240607
(defmethod process-end-headers :after (connection (stream gzip-decoding-mixin))
  (setf (get-dstate stream) (chipz:make-dstate 'chipz:gzip)))

(defmethod apply-data-frame :around ((stream gzip-decoding-mixin) payload start end)
  ;; 20240607 TODO: Window update should go elsewhere

  (write-window-update-frame (http2/core::get-connection stream) (- end start))
  (write-window-update-frame stream (- end start))
  ;; 20240607 TODO: test on dstate, not on header

  (if (equal (cdr (assoc "content-encoding" (get-headers stream) :test 'equal)) "gzip")
      (let* ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
        (loop
          (multiple-value-bind (consumed produced)
              (chipz:decompress buffer (get-dstate stream) payload :input-start start
                                                                   :input-end end)
            (when (and (zerop consumed) (zerop produced)) (return))
            (when (plusp produced)
              (call-next-method stream buffer 0 produced))
            (incf start consumed))))
      (call-next-method)))
