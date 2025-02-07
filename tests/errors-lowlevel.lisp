(in-package :http2/core)

(fiasco:defsuite
    (http2/tests/core :bind-to-package #:http2/core
                 :in http2/tests::http2/tests))


(defmacro with-test-client-to-server-setup (&body body)
  `(let (#+nil(sender (make-instance 'vanilla-client-connection :network-stream (make-broadcast-stream)))
         (receiver (make-instance 'http2/server::vanilla-server-connection :network-stream (make-broadcast-stream))))
     ,@body))

(defvar *payloads* (make-hash-table))

(defmacro create-test-payload (name (&optional (connection-name (gensym)))
                                      &body body)

  `(setf (gethash ',name *payloads* )
         (let ((,connection-name (make-instance 'http2/client:vanilla-client-connection
                                                :network-stream (make-broadcast-stream))))
           ,@body)))

(create-test-payload end-of-file (conn)
  (write-frame conn
               -1 +data-frame+
               (list :padded (make-octet-buffer 10))
               (constantly nil) #()))


(create-test-payload too-big-padding ()
  (http2/utils:make-initialized-octet-buffer #(0 0 10 1 8 0 0 0 1 0 0 0 0 0 0 0 0 0 0)))

(create-test-payload null-connection-window-update (conn)
  (write-window-update-frame conn 0))

(create-test-payload null-stream-window-update (conn)
  (let ((new-stream (create-new-local-stream conn)))
    (write-headers-frame new-stream
                         (compile-headers (http2/client::request-headers "GET" "/" "localhost") nil)
                         :end-headers t)
    (write-window-update-frame new-stream 0)))

(defun data-should-cause-error (data-name)
  (with-test-client-to-server-setup
    (loop with data = (gethash data-name *payloads*)
          with start = 0 and end = (length data)
          with fn = #'parse-frame-header
          with size = 9
          while (> end start)
          do
             (multiple-value-setq (fn size) (funcall fn receiver data start (+ start size)))
             (incf start size))))

(create-test-payload bad-stream-state (conn)
  (write-headers-frame
   (make-instance (get-stream-class conn)
                  :stream-id 3
                  :connection conn
                  :state 'open)
   (compile-headers (http2/client::request-headers "GET" "/" "localhost") nil)
     :end-headers t)
  (write-headers-frame
   (make-instance (get-stream-class conn)
                  :stream-id 1
                  :connection conn
                  :state 'open)
   (compile-headers (http2/client::request-headers "GET" "/" "localhost") nil)
   :end-headers t))

(fiasco:deftest low-level-errors ()
  ;; make payload smaller than padding
  (fiasco:signals too-big-padding (data-should-cause-error 'too-big-padding))
  #+nil  (fiasco:signals end-of-file (data-should-cause-error 'end-of-file))
  (fiasco:signals null-connection-window-update (data-should-cause-error 'null-connection-window-update))
  (fiasco:signals null-stream-window-update (data-should-cause-error 'null-stream-window-update))
  (fiasco:signals bad-stream-state (data-should-cause-error 'bad-stream-state)))

(fiasco:deftest error/null-connection-window-update ()
  (fiasco:signals null-stream-window-update
    (with-test-client-to-server-setup
      (let ((new-stream (create-new-local-stream sender)))
        (write-headers-frame new-stream
                             (request-headers "GET" "/" "localhost")
                             :end-headers t)
        (write-window-update-frame new-stream 0)
        (read-frame receiver)
        (read-frame receiver)))))

(fiasco:deftest error/bad-stream-state ()
  (with-test-client-to-server-setup
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
     (request-headers "GET" "/" "localhost")
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
       (request-headers "GET" "/" "localhost")
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
      (http2/core::write-frame-header write-stream (length (car (request-headers "GET" "/" "localhost")))
                          1 (flags-to-code '(:end-headers t)) stream t)
      (write-sequences write-stream (request-headers "GET" "/" "localhost"))
      (fiasco:signals reserved-bit-set
        (read-frame receiver)))))
(fiasco:deftest error/end-of-file ()
  (fiasco:signals end-of-file
    (data-should-cause-error 'end-of-file)
    http2/core::(with-test-client-to-server-setup
                  (let ((stream (create-new-local-stream sender)))
                    (parse-frame-header receiver
                                        (write-frame stream
                                                     0 +headers-frame+ (list :end-headers t)
                                                     (constantly nil) #())
                                        0 9)
                    (http2/core::parse-frame-header receiver
                                                    (http2/core::write-frame stream
                                                                             -1 +data-frame+
                                                                             (list :padded (make-octet-buffer 10))
                                                                             (constantly nil) #())
                                                    0 9)))))
