(in-package http2)
;;;; Implement pipes for testing client-server pairs.
(defclass binary-stream ()
  ())

(defmethod stream-element-type (binary-stream)
  '(unsigned-byte 8))

(defclass pipe-end-for-read (trivial-gray-streams:fundamental-binary-input-stream binary-stream)
  ((buffer :accessor get-buffer :initarg :buffer)
   (index  :accessor get-index  :initarg :index)))

(defclass pipe-end-for-write (trivial-gray-streams:fundamental-binary-output-stream binary-stream)
  ((buffer :accessor get-buffer :initarg :buffer)))

(defun make-pipe (&key (buffer-size 4096))
  (let ((buffer (make-array buffer-size :adjustable t
                                        :fill-pointer 0)))
    (values (make-instance 'pipe-end-for-write
                                       :buffer buffer)
            (make-instance 'pipe-end-for-read :buffer buffer
                           :index 0))))

(defmethod trivial-gray-streams:stream-write-byte ((stream pipe-end-for-write) byte)
  (vector-push-extend byte (get-buffer stream)))

(defmethod trivial-gray-streams:stream-read-byte ((stream pipe-end-for-read))
  (if (= (get-index stream) (length (get-buffer stream))) :eof
      (prog1 (aref (get-buffer stream) (get-index stream))
        (incf (get-index stream)))))

(defmethod trivial-gray-streams:stream-listen (stream)
  (< (get-index stream) (length (get-buffer stream))))

(defclass logging-object ()
  ((reversed-history :accessor get-reversed-history :initarg :reversed-history))
  (:default-initargs :reversed-history nil))

(defclass logging-connection (http2-connection logging-object)
  ())

(defclass logging-stream (http2-stream logging-object)
  ())

(defun add-log (object log-pars)
  (push log-pars (get-reversed-history object)))

(defun get-history (object)
  (reverse (get-reversed-history object)))

(defmethod apply-data-frame ((connection logging-connection) (stream logging-stream)
                             payload)
  (add-log stream `(:payload ,payload)))

(defmethod add-header :before ((stream logging-stream) name value)
  (add-log stream `(:header ,name ,value)))

(defun make-dummy-connection (stream &key (class 'logging-connection)
                                       (STREAM-ID 1) (STATE 'open))
  "Make a dummy connection class with one stream of id ID in state STATE."
  (make-instance class
                 :network-stream stream
                 :streams (list (make-instance 'logging-stream
                                               :stream-id stream-id
                                               :state STATE))))

(defmacro with-sender-receiver (() &body body)
  `(multiple-value-bind (write-stream read-stream) (make-pipe)
     (let ((sender (make-dummy-connection write-stream))
           (receiver (make-dummy-connection read-stream)))
       ,@body)))

(defun test-one-frame (send-fn &rest send-pars)
  (with-sender-receiver ()
    (apply send-fn sender (make-instance 'http2-stream :stream-id 1) send-pars)
    (read-frame receiver)
    (cons
     (get-history receiver)
     (get-history (car (get-streams receiver))))))

(stefil:deftest test-frames ()
  (stefil:is (equalp (test-one-frame #'write-data-frame #(1 2 3 4 5))
                     '(NIL (:PAYLOAD #(1 2 3 4 5))))
      "Payload not processed")
  (stefil:is (equalp (test-one-frame #'write-headers-frame
                                     (list (encode-header "foo" "bar")
                                           (encode-header :path "/")
                                           (encode-header "baz" "bah")))
                     '(NIL (:header :path "/")))
      "Header frames do not match.")
  nil)
