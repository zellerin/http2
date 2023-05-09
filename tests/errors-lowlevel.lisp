(in-package :http2)

(fiasco:deftest too-big-frame/test ()
  ;; default max-frame-size is 16384, 0x4000
  (fiasco:signals too-big-frame
    (multiple-value-bind (write-stream read-stream) (make-full-pipe)
      (let ((sender (make-dummy-connection write-stream))
            (receiver (make-dummy-connection read-stream)))
        (write-frame sender 65536 0 nil (lambda (a) (write-sequence (make-array 65536 :initial-element 0) a)))
        (read-frame receiver)))))

(fiasco:deftest too-big-padding/test ()
  ;; make payload smaller than padding
  (fiasco:signals too-big-padding
    (multiple-value-bind (write-stream read-stream) (make-full-pipe)
      (let ((sender (make-dummy-connection write-stream))
            (receiver (make-dummy-connection read-stream)))
        (write-frame
         (create-new-local-stream sender)
         -1 +data-frame+
         (list :padded (make-array 10 :element-type '(unsigned-byte 8)))
         (constantly nil) #())
        (read-frame receiver))))
  (fiasco:signals too-big-padding
    (multiple-value-bind (write-stream read-stream) (make-full-pipe)
      (let ((sender (make-dummy-connection write-stream))
            (receiver (make-dummy-connection read-stream)))
        (write-frame
         (create-new-local-stream sender)
         -1 +headers-frame+
         (list :padded (make-array 10 :element-type '(unsigned-byte 8)))
         (constantly nil) #())
        (read-frame receiver)))))
