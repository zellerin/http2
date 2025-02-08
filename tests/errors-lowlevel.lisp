(in-package :http2/core)

(fiasco:defsuite
    (http2/tests/core :bind-to-package #:http2/core
                 :in http2/tests::http2/tests))


(defmacro with-test-client-to-server-setup (&body body)
  `(let (#+nil(sender (make-instance 'vanilla-client-connection :network-stream (make-broadcast-stream)))
         (receiver (make-instance 'http2/server::vanilla-server-connection :network-stream (make-broadcast-stream))))
     ,@body))

(defvar *payloads* (make-hash-table))

(defun get-first-line (text)
  (with-input-from-string (in text)
    (read-line in)))

(defstruct (payload (:print-object (lambda (object stream) (format stream "<Payload: ~a>" (get-first-line (payload-documentation object))))))
  (documentation "N/A" :type string)
  (code (make-octet-buffer 0) :type octet-vector)
  (test (constantly nil) :type compiled-function))

(defun process-data-by-connection (data-name)
  "Parse data sample named by DATA-NAME till the end."
  (with-test-client-to-server-setup
    (loop with data of-type octet-vector = (payload-code (gethash data-name *payloads*))
          with start of-type frame-size = 0 and end of-type frame-size = (length data)
          with fn of-type receiver-fn = #'parse-frame-header
          with size of-type frame-size = 9
          for old-size of-type frame-size = size
          while (> end start)
          do
             (multiple-value-setq (fn size) (funcall fn receiver data start (min end (+ start size))))
             (incf start old-size))))

(defun test-for-error (payload-name error-name)
  (declare (optimize debug))
  (handler-case
      (progn
        (process-data-by-connection payload-name)
        (fiasco:is nil "Error ~a expected for payload ~a" error-name payload-name))
    (error (e)
      (fiasco:is (equal (type-of e) error-name)
          "Error ~a was expected, but got ~a instead" error-name e)
      e)))

(defmacro define-test-payload (name (&optional (connection-name))
                               &body body)
  `(setf (gethash ',name *payloads* )
         (make-payload
          :documentation ,(if (stringp (car body)) (pop body) "N/A")
          :code
          (let ,(when connection-name
                  `((,connection-name (make-instance 'http2/client:vanilla-client-connection
                                                     :network-stream (make-broadcast-stream)))))
            ,@body))))


(define-test-payload end-of-file (conn)
  "Frame should have 10 octets, but has just 9"
  (write-frame conn -1 +data-frame+ (list :padded (make-octet-buffer 10))
               (constantly nil) #()))


(define-test-payload too-big-padding ()
  "Padding leaves no space for even empty real content"
  (http2/utils:make-initialized-octet-buffer #(0 0 10 1 8 0 0 0 1 10 0 0 0 0 0 0 0 0 0)))

(define-test-payload null-connection-window-update (conn)
  "Send empty update to a connection"
  (write-window-update-frame conn 0))

(fiasco:deftest low-level-errors ()
  (maphash (lambda (key val)
             (declare (ignore val))
             (test-for-error key key))
           *payloads*))

(defmacro with-new-stream ((stream-name) &body body)
  `(let ((,stream-name (create-new-local-stream conn)))
     (concatenate 'octet-vector
                  (write-headers-frame ,stream-name
                                       (compile-headers (http2/client::request-headers "GET" "/" "localhost") nil)
                                       :end-headers t)
                  ,@body)))

(define-test-payload null-stream-window-update (conn)
  "Send empty update to a stream"
  (with-new-stream (new-stream)
    (write-window-update-frame new-stream 0)))



(define-test-payload bad-stream-state (conn)
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
  (fiasco:signals too-big-padding (process-data-by-connection 'too-big-padding))
  #+nil  (fiasco:signals end-of-file (process-data-by-connection 'end-of-file))
  (fiasco:signals null-connection-window-update (process-data-by-connection 'null-connection-window-update))
  (fiasco:signals null-stream-window-update (process-data-by-connection 'null-stream-window-update))
  (fiasco:signals bad-stream-state (process-data-by-connection 'bad-stream-state)))

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
    (process-data-by-connection 'end-of-file)
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
