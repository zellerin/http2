(in-package :http2)

(defmacro with-test-harness (&body body)
  ` (multiple-value-bind (write-stream read-stream) (make-full-pipe)
      (let ((sender (make-instance 'vanilla-client-connection
                                   :network-stream write-stream))
            (receiver (make-instance 'vanilla-server-connection
                                     :network-stream read-stream)))
        ,@body)))

(fiasco:deftest too-big-frame/test ()
  ;; default max-frame-size is 16384, 0x4000
  (fiasco:signals too-big-frame
    (with-test-harness
      (write-frame sender 65536 0 nil
                   (lambda (a) (write-sequence (make-array 65536 :initial-element 0) a)))
      (read-frame receiver))))

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

(fiasco:deftest null-window-increments/test ()
  (fiasco:signals null-connection-window-update
    (multiple-value-bind (write-stream read-stream) (make-full-pipe)
        (let ((sender (make-dummy-connection write-stream))
              (receiver (make-dummy-connection read-stream)))
          (write-window-update-frame sender 0)
          (read-frame receiver))))

  (fiasco:signals null-stream-window-update
    (multiple-value-bind (write-stream read-stream) (make-full-pipe)
      (let ((sender (make-dummy-connection write-stream))
            (receiver (make-dummy-connection read-stream)))
        (write-window-update-frame (create-new-local-stream sender) 0)
        (read-frame receiver)))))

(fiasco:deftest open-implicitly-closed-stream ()
  (with-test-harness
    (write-headers-frame
     (make-instance (get-stream-class sender)
            :stream-id 3
            :connection sender
            :network-stream (get-network-stream sender)
            :state 'open)
     nil)
    (read-frame receiver)
    (write-headers-frame
     (make-instance (get-stream-class sender)
                    :stream-id 1
                    :connection sender
                    :network-stream (get-network-stream sender)
                    :state 'open)
     nil)
    (read-frame receiver)))
