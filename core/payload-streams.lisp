(in-package http2)

;;;; What does "stream" mean here?
;;;;
;;;; 1. we have TCP socket (usocket) and possibly stream as well
;;;; 2. over it we have TLS socket and TLS stream (cl+ssl)
;;;; 3. over it we run http connection with http streams
;;;; 4. over the http stream of type data we create a binary stream
;;;; 5. and over it we have a flexi stream with some encoding.
;;;;
;;;; This file is about defining (4) based on (3).

(defclass binary-stream ()
  ())

(defmethod stream-element-type ((stream binary-stream))
  '(unsigned-byte 8))

(defclass binary-output-stream-over-data-frames (binary-stream trivial-gray-streams:fundamental-binary-output-stream)
  ((output-buffer   :accessor get-output-buffer)
   (send-threshold  :accessor get-send-threshold  :initarg :send-threshold)
   (http-stream     :accessor get-http-stream     :initarg :http-stream)
   (http-connection :accessor get-http-connection :initarg :http-connection))
  (:default-initargs :to-write 0 :to-store 0 :send-threshold 4096)
  (:documentation
   "Binary stream that accepts new octets to the output-buffer, until it is big
enough to send the data as a data frame (or forced to by close of force-output)."))

(defmethod initialize-instance :after ((stream binary-output-stream-over-data-frames)
                                       &key http-stream (window-size (get-peer-window-size http-stream)) send-threshold  &allow-other-keys)
  (setf (get-output-buffer stream)
        (make-array window-size :element-type '(unsigned-byte 8)
                                :fill-pointer 0 :adjustable nil)
        (get-send-threshold stream) (or send-threshold
                                        (floor window-size 3)))) ; first heuristics

(defmethod trivial-gray-streams:stream-write-byte ((stream binary-output-stream-over-data-frames) byte)
  (with-slots (output-buffer http-stream http-connection) stream
    (if (< (fill-pointer output-buffer) (array-dimension output-buffer 0))
        (vector-push byte output-buffer)
        (warn "Not enough space in buffer: ~d<~d"
               (fill-pointer output-buffer) (array-dimension output-buffer 0)))
    (when (> (min (fill-pointer output-buffer))
             (get-send-threshold stream))
      (loop while (<  (min (get-peer-window-size http-stream)
                           (get-peer-window-size http-connection))
                      (length output-buffer))
            ;; we want to send more than window allows, so lets wait for more
            ;; window
            ;; this assumes that the client will not shrink the window too much.
            do (read-frame http-connection))
      (write-data-frame http-connection http-stream output-buffer)
      (setf (fill-pointer output-buffer) 0))))

(defmethod close ((stream binary-output-stream-over-data-frames) &key &allow-other-keys)
  (with-slots (output-buffer http-stream http-connection) stream
    (write-data-frame http-connection http-stream output-buffer :end-stream t)))

(defmethod trivial-gray-streams:stream-force-output ((stream binary-output-stream-over-data-frames))
  (with-slots (output-buffer http-stream http-connection) stream
    (write-data-frame http-connection http-stream output-buffer :end-stream nil)))

;; TODO: finish-output could wait for window updates arriving. Except afaics
;; noone forces the other side to keep window size unchanged over time...

(defmethod trivial-gray-streams:stream-write-sequence
    ((stream binary-output-stream-over-data-frames) sequence start end &key)
  ;; this is here for profiling at the moment. Implementing better version of
  ;; this would be beneficial for performance if it is used.
  (with-slots (http-connection http-stream output-buffer send-threshold) stream
    (flet ((send-data (size)
             "Send data in OUTPUT-BUFFER and SIZE data from SEQUENCE starting at START in one
              data frame; mark them as sent."
             (write-data-frame http-connection http-stream
                               (if (zerop (length output-buffer))
                                   (make-array size
                                               :displaced-to sequence
                                               :displaced-index-offset start
                                               :element-type '(unsigned-byte 8))
                                   (list output-buffer
                                                 (make-array size
                                                             :displaced-to sequence
                                                             :displaced-index-offset start
                                                             :element-type '(unsigned-byte 8)))))
             (setf (fill-pointer output-buffer) 0)
             (incf start size)))

      (let ((total-length (- (+ (or end (length sequence))
                                (length (get-output-buffer stream)))
                             start)))
        (loop
          for allowed-window = (min (get-peer-window-size http-connection)
                                            (get-peer-window-size http-stream))
          while (> total-length allowed-window)
          do (send-data (- allowed-window (length output-buffer)))
             (decf total-length allowed-window)
             (read-frame http-connection))
        (if (> total-length send-threshold)
            ;; we send it and go on
            (send-data (length sequence))
            ;; just copy data to the output-buffer
            (loop for idx from start to (1- end)
                  do (vector-push (aref sequence idx) output-buffer)))))))
