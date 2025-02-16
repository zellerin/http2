(in-package :http2)

(fiasco:deftest too-big-frame/test ()
  ;; default max-frame-size is 16384, 0x4000
  (fiasco:signals too-big-frame
    (with-test-client-to-server-setup
      (read-frames-from-octets receiver
                               (write-frame sender 65536 +settings-frame+ nil
                                            (lambda (a start)
                                              (fill a 0 :start start)))
                               0 65536))))

(fiasco:deftest error/too-big-padding ()
  ;; make payload smaller than padding
  (fiasco:signals end-of-file
    (with-test-client-to-server-setup
      (read-frame-from-octets receiver
                              (write-frame
                               (create-new-local-stream sender)
                               -1 +headers-frame+
                               (list :padded (make-octet-buffer 10))
                               (constantly nil) #())
                              0 9))))

(fiasco:deftest error/end-of-file ()
    (fiasco:signals end-of-file
      (with-test-client-to-server-setup
        (let ((stream (create-new-local-stream sender)))
          (read-frame-from-octets
           receiver
           (write-frame stream
                        0 +headers-frame+ (list :end-headers t)
                        (constantly nil) #())
           0 9)
          (read-frame-from-octets receiver
                                  (write-frame stream
                                               -1 +data-frame+
                                               (list :padded (make-octet-buffer 10))
                                               (constantly nil) #())
                                  0 9)))))

(fiasco:deftest error/null-connection-window-update ()
  (fiasco:signals null-connection-window-update
    (with-test-client-to-server-setup
      (write-window-update-frame sender 0)
      (read-frame receiver)))

  (fiasco:signals null-stream-window-update
    (with-test-client-to-server-setup
      (let ((new-stream (create-new-local-stream sender)))
        (write-headers-frame new-stream
                             (http2/hpack:request-headers "GET" "/" "localhost")
                             :end-headers t)
        (write-window-update-frame new-stream 0)
        (read-frame receiver)
        (read-frame receiver)))))

(fiasco:deftest error/bad-stream-state ()
  (with-test-client-to-server-setup
    (write-headers-frame
     (make-instance (get-stream-class sender)
                    :stream-id 3
                    :connection sender
                    :state 'open)
     (http2/hpack:request-headers "GET" "/" "localhost")
     :end-headers t)
    (read-frame receiver)
    (write-headers-frame
     (make-instance (get-stream-class sender)
                    :stream-id 1
                    :connection sender
                    :state 'open)
     (http2/hpack:request-headers "GET" "/" "localhost")
     :end-headers t)
    (fiasco:signals bad-stream-state
      (read-frame receiver))))

(fiasco:deftest error/bad-stream-state/2 ()
  (with-test-client-to-server-setup
    (write-data-frame
     (make-instance (get-stream-class sender)
                    :stream-id 1
                    :connection sender
                    :state 'open)
     #())
    (fiasco:signals bad-stream-state
      (read-frame receiver))))

(fiasco:deftest error/frame-type-needs-stream ()
  (with-test-client-to-server-setup
    (write-headers-frame
     sender
     (http2/hpack:request-headers "GET" "/" "localhost")
     :end-headers t)
    (fiasco:signals frame-type-needs-stream
      (read-frame receiver))))

(fiasco:deftest error/frame-type-needs-connection ()
  ""
  (with-test-client-to-server-setup
    (let ((stream (make-instance (get-stream-class sender)
                                 :stream-id 1
                                 :connection sender
                                 :state 'open)))
      (write-headers-frame
       stream
       (http2/hpack:request-headers "GET" "/" "localhost")
       :end-headers t)
      (read-frame receiver)
      (write-settings-frame stream nil))
    (fiasco:signals frame-type-needs-connection
      (read-frame receiver))))

(fiasco:deftest error/reserved-bit-set ()
  ""
  (with-test-client-to-server-setup
    (let ((stream (make-instance (get-stream-class sender)
                                 :stream-id 1
                                 :connection sender
                                 :state 'open)))
      ;; we do not allow to write R until we go to the lowest level
      (write-frame-header write-stream (length (car (http2/hpack:request-headers "GET" "/" "localhost")))
                          1 (flags-to-code '(:end-headers t)) stream t)
      (write-sequences write-stream (http2/hpack:request-headers "GET" "/" "localhost"))
      (fiasco:signals reserved-bit-set
        (read-frame receiver)))))
