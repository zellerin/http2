(in-package :http2)

(defmacro with-test-harness (&body body)
  `(multiple-value-bind (write-stream read-stream) (make-full-pipe)
     (let ((sender (make-instance 'vanilla-client-connection
                                  :network-stream write-stream))
            (receiver (make-instance 'vanilla-server-connection
                                     :network-stream read-stream)))
       (read-client-preface receiver)
       (read-frame receiver) ; settings
       ,@body)))

(fiasco:deftest too-big-frame/test ()
  ;; default max-frame-size is 16384, 0x4000
  (fiasco:signals too-big-frame
    (with-test-harness
      (write-frame sender 65536 0 nil
                   (lambda (a) (write-sequence (make-array 65536 :initial-element 0) a)))
      (read-frame receiver)
      (break))))

(fiasco:deftest too-big-padding/test ()
  ;; make payload smaller than padding
  (fiasco:signals too-big-padding
    (with-test-harness
      (write-frame
       (create-new-local-stream sender)
       -1 +headers-frame+
       (list :padded (make-array 10 :element-type '(unsigned-byte 8)))
       (constantly nil) #())
      (read-frame receiver)
      (read-frame receiver)))

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
    (with-test-harness
        (write-window-update-frame sender 0)
        (read-frame receiver)))

  (fiasco:signals null-stream-window-update
    (with-test-harness
      (let ((new-stream (create-new-local-stream sender)))
        (write-headers-frame new-stream
                             (http2/hpack:request-headers "GET" "/" "localhost")
                             :end-headers t)
        (write-window-update-frame new-stream 0)
        (read-frame receiver)
        (read-frame receiver)))))

(fiasco:deftest open-implicitly-closed-stream ()
  (with-test-harness
    (write-headers-frame
     (make-instance (get-stream-class sender)
                    :stream-id 3
                    :connection sender
                    :network-stream (get-network-stream sender)
                    :state 'open)
     (http2/hpack:request-headers "GET" "/" "localhost")
     :end-headers t)
    (read-frame receiver)
    (write-headers-frame
     (make-instance (get-stream-class sender)
                    :stream-id 1
                    :connection sender
                    :network-stream (get-network-stream sender)
                    :state 'open)
     (http2/hpack:request-headers "GET" "/" "localhost")
     :end-headers t)
    (fiasco:signals bad-stream-state
      (read-frame receiver))))

(fiasco:deftest test-continuation-header ()
  "Let us take a header and try to split it on all places."
  (loop with headers = (car (http2/hpack:request-headers "GET" "/" "localhost"))
        for split-idx from 0 to (length headers)
        do
           (with-test-harness
             (let ((new-stream (create-new-local-stream sender)))
               (write-headers-frame new-stream
                                    (list (subseq headers 0 split-idx))
                                    :end-headers nil)
               (write-continuation-frame new-stream
                                         (list (subseq headers split-idx ))
                                         :end-headers t)
               (read-frame receiver)
               (when (listen read-stream)
                 ;; the frame might have already been consumed
                 (read-frame receiver))
               (let ((received-stream (car (get-streams receiver)) ))
                 (fiasco:is
                     (equal "/" (get-path received-stream)))
                 (fiasco:is
                     (equal "GET" (get-method received-stream)))
                 (fiasco:is
                     (eq 'open (get-state received-stream))))))))
