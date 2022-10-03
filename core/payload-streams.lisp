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
  ((output-buffer   :accessor get-output-buffer))
  (:default-initargs :to-write 0 :to-store 0)
  (:documentation
   "Binary stream that accepts new octets to the output-buffer, until it is big
enough to send the data as a data frame (or forced to by close of force-output)."))

(defmethod initialize-instance :after ((stream binary-output-stream-over-data-frames)
                                       &key connection
                                         (window-size (get-initial-peer-window-size connection))  &allow-other-keys)
  (setf (get-output-buffer stream)
        (make-array window-size :element-type '(unsigned-byte 8)
                                :fill-pointer 0 :adjustable nil)))

(defmethod trivial-gray-streams:stream-write-byte ((stream binary-output-stream-over-data-frames) byte)
  (warn "this is actually untested and was written some time ago, so might be buggy.")

  (with-slots (output-buffer connection) stream
    (if (< (fill-pointer output-buffer) (array-dimension output-buffer 0))
        (vector-push byte output-buffer)
        (warn "Not enough space in buffer: ~d<~d"
               (fill-pointer output-buffer) (array-dimension output-buffer 0)))
    (when (>= (min (fill-pointer output-buffer))
              (get-max-peer-frame-size connection))
      (loop while (<  (min (get-peer-window-size stream)
                           (get-peer-window-size connection))
                      (length output-buffer))
            ;; we want to send more than window allows, so lets wait for more
            ;; window
            ;; this assumes that the client will not shrink the window too much.
            do
               (read-frame connection))
      (write-data-frame stream output-buffer)
      (setf (fill-pointer output-buffer) 0))))

(defmethod close ((stream binary-output-stream-over-data-frames) &key &allow-other-keys)
  (with-slots (output-buffer connection) stream
    ;; FIXME: here we know (aside of intervening setting changes) that the
    ;; output buffer is smaller than max-frame-size, but it still might be
    ;; larger than the window.

    ;; Should we handle also RST on send?
    (unless (eq 'closed (get-state stream))
      (write-data-frame stream output-buffer :end-stream t)
      (force-output (get-network-stream connection)))))

(defun close-output (stream)
  (with-slots (output-buffer connection) stream
    ;; FIXME: here we know (aside of intervening setting changes) that the
    ;; output buffer is smaller than max-frame-size, but it still might be
    ;; larger than the window.

    ;; Should we handle also RST on send?
    (write-data-frame stream output-buffer :end-stream t)
    (force-output (get-network-stream connection))))

(defmethod trivial-gray-streams:stream-force-output ((stream binary-output-stream-over-data-frames))
  (with-slots (output-buffer connection) stream
    (unless (zerop (length output-buffer))
      (write-data-frame stream output-buffer :end-stream nil)
      (setf (fill-pointer output-buffer) 0))
    (force-output (get-network-stream connection))))

;; TODO: finish-output could wait for window updates arriving. Except afaics
;; noone forces the other side to keep window size unchanged over time...


(defun send-data (stream sequence start size)
  "Send data in OUTPUT-BUFFER and SIZE data from SEQUENCE starting at START in
  one data frame; mark them as sent.

Return new START."
  (with-slots (output-buffer) stream
    (write-data-frame stream
                      (if (zerop (length output-buffer))
                          (make-array size
                                      :displaced-to sequence
                                      :displaced-index-offset start
                                      :element-type '(unsigned-byte 8))
                          (list output-buffer
                                (make-array size
                                            :displaced-to sequence
                                            :displaced-index-offset start
                                            :element-type '(unsigned-byte 8))))))
  (setf (fill-pointer (get-output-buffer stream)) 0)
  (incf start size))

(defmethod trivial-gray-streams:stream-write-sequence
    ((stream binary-output-stream-over-data-frames) sequence start end &key)
  (with-slots (connection output-buffer send-threshold) stream
    ;; Situations:
    ;; - We have more than max-peer-frame-size data, and peer window is above it -> we can send a frame and go on
    ;; - We do not have enough data (full frame) yet - we wait for more data
    ;; - We have more than max-peer-frame-size, but window is too small -> we read a frame

    (let ((total-length (- (+ (or end (length sequence))
                              (length (get-output-buffer stream)))
                           start)))
      (loop
        for allowed-window = (min (get-peer-window-size connection)
                                  (get-peer-window-size stream))
        and frame-size = (get-max-peer-frame-size connection)
        while (and (>= total-length frame-size))
        do (cond ((>= allowed-window frame-size)
                  (setf start
                        (send-data stream sequence start (- frame-size (length output-buffer))))
                  (decf total-length frame-size))
                 (t
                  (read-frame connection))))
      (loop for idx from start to (1- end)
            do (vector-push (aref sequence idx) output-buffer)))))

(defclass binary-input-stream-over-data-frames (binary-stream trivial-gray-streams:fundamental-binary-input-stream)
  ((index           :accessor get-index           :initarg :index)
   (data       :accessor get-data       :initarg :data
               :documentation
               "tlist that of accepted frames and cons of last frame and nil."))
  (:default-initargs :index 0 :data (cons nil nil))
  (:documentation
   "Binary stream that reads data from the http stream.

It keeps data from last data frame in BUFFER, starting with INDEX."))

(defun empty-data-p (data)
  (null (car data)))

(defun push-frame (data frame)
  (if (empty-data-p data)
      (setf (car data) (cons frame nil)
            (cdr data) (car data))
      (setf (cddr data) (cons frame nil)
            (cdr data) (cddr data)))
  data)

(defun pop-frame (data)
  (pop (car data)))

(defmethod apply-data-frame ((stream binary-input-stream-over-data-frames) frame-data)
  (push-frame (get-data stream) frame-data))

(defmethod trivial-gray-streams:stream-listen ((stream binary-input-stream-over-data-frames))
  (force-output (get-network-stream stream))
  (with-slots (connection index data to-store to-provide state) stream
     (or (caar data)
         ;; we assume we would not get just part of the frame.
         (loop while (listen (get-network-stream connection))
               do (read-frame connection)
               when (member state '(closed half-closed/remote))
                 do (return nil)
               when (caar data) do (return t)))))

(defmethod trivial-gray-streams:stream-read-byte ((stream binary-input-stream-over-data-frames))
  (force-output (get-network-stream stream))
  (with-slots (connection index data to-store to-provide state) stream
    (loop for buffer = (caar data)
          until (or buffer (member state '(closed half-closed/remote)))
          do
             ;; read-frame -> push frame to the data or close the buffer
             (if (listen (get-network-stream connection))
                 (read-frame connection)
                 (sleep 0.1))
          finally
             (return
               (if (and (null buffer) (member state '(closed half-closed/remote))) :eof
                           (prog1 (aref buffer index)
                             (when (= (incf index) (length (caar data)))
                               (pop-frame data)
                               (add-log connection `(:window-size-increment ,index))
                               (add-log stream `(:window-size-increment ,index))
                               (write-window-update-frame connection index)
                               (write-window-update-frame stream index)
                               (setf index 0))))))))
