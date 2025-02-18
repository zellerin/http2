(in-package http2/stream-overlay)

;;;; What does "stream" mean here?
;;;;
;;;; 1. we have TCP socket (usocket) and possibly stream as well
;;;; 2. over it we have TLS socket and TLS stream (cl+ssl)
;;;; -> network stream
;;;; 3. over it we run http connection with http streams
;;;; -> http2 stream
;;;; 4. over the http stream of type data we create a binary stream
;;;; -> payload stream
;;;; 5. possibly a stream that implemengts content-encoding
;;;; 6. and over it we have a flexi stream with charset encoding
;;;; -> transport stream
;;;;

(mgl-pax:defsection @overlay
    ()
  (http2-stream-with-input-stream class))

(defmethod stream-element-type ((stream binary-stream))
  '(unsigned-byte 8))


(defclass payload-stream (binary-stream)
  ((base-http2-stream :accessor get-base-http2-stream :initarg :base-http2-stream))
  (:documentation
   "Base class for a CL binary stream that is defined over http2 stream"))

(defclass payload-output-stream (payload-stream trivial-gray-streams:fundamental-binary-output-stream)
  ((output-buffer   :accessor get-output-buffer))
  (:default-initargs :to-write 0 :to-store 0)
  (:documentation
   "Binary stream that accepts new octets to the output-buffer, until it is big
enough to send the data as a data frame on BASE-HTTP2-STREAM (or forced to by close of force-output) "))

(defmethod initialize-instance :after ((stream payload-output-stream)
                                       &key
                                         base-http2-stream
                                         (connection (http2/core::get-connection base-http2-stream))
                                         (window-size (min 65536 (http2/core::get-initial-peer-window-size connection)))  &allow-other-keys)
  (setf (get-output-buffer stream)
        (make-array window-size :element-type '(unsigned-byte 8)
                                :fill-pointer 0 :adjustable nil)))

(defmacro with-output-payload-slots (stream &body body)
  `(with-slots (output-buffer base-http2-stream) ,stream
     (with-slots (connection peer-window-size state) base-http2-stream
       ,@body)))

(define-condition http2-write-data-stall (warning)
  ((sent :reader get-sent :initarg :sent)
   (data :reader get-data :initarg :data))
  (:documentation "Signalled when data are to be sent and there is not big enough window available
to sent. Tracks DATA to sent and number of octets actually SENT."))

(defmethod trivial-gray-streams:stream-write-byte ((stream payload-output-stream) byte)
  (with-output-payload-slots stream
    (if (< (fill-pointer output-buffer) (array-dimension output-buffer 0))
        (vector-push byte output-buffer)
        (warn "Not enough space in buffer: ~d<~d"
              (fill-pointer output-buffer) (array-dimension output-buffer 0)))
    (when (>= (min (fill-pointer output-buffer))
              (http2/core::get-max-peer-frame-size connection))
      (loop while (<  (min peer-window-size
                           (http2/core::get-peer-window-size connection))
                      (length output-buffer))
            ;; we want to send more than window allows, so lets wait for more
            ;; window
            ;; this assumes that the client will not shrink the window too much.
            do (read-frame connection))
      (write-data-frame base-http2-stream output-buffer)
      (setf (fill-pointer output-buffer) 0))))

(defmethod close ((stream payload-output-stream) &key &allow-other-keys)
  (with-output-payload-slots stream
      ;; FIXME: here we know (aside of intervening setting changes) that the
      ;; output buffer is smaller than max-frame-size, but it still might be
      ;; larger than the window.

      ;; Should we handle also RST on send?
      (unless (eq 'closed state)
        (write-data-frame base-http2-stream output-buffer :end-stream t)
#+not-suitable-for-some        (force-output (get-network-stream connection)))))

(defmethod trivial-gray-streams:stream-force-output ((stream payload-output-stream))
  (with-output-payload-slots stream
    (unless (zerop (length output-buffer))
      (write-data-frame base-http2-stream output-buffer :end-stream nil)
      (setf (fill-pointer output-buffer) 0))
    (force-output (get-network-stream connection))))

;; TODO: finish-output could wait for window updates arriving. Except afaics
;; noone forces the other side to keep window size unchanged over time...

(defun send-data (stream sequence start size)
  "Send data in OUTPUT-BUFFER and SIZE data from SEQUENCE starting at START in
  one data frame; mark them as sent.

Return new START."
  (with-slots (output-buffer base-http2-stream) stream
    (write-data-frame-multi base-http2-stream
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

(defun wait-for-window-is-at-least-frame-size (connection http-stream)
  (loop for allowed-window = (min (get-peer-window-size connection)
                                  (get-peer-window-size http-stream))
        for frame-size = (get-max-peer-frame-size connection)
        while (> frame-size allowed-window)
        do (read-frame connection)
#+nil(loop until
       (restart-case

           (read-again ())))))

(defmethod trivial-gray-streams:stream-write-sequence
    ((stream payload-output-stream) sequence start end &key)
  (with-output-payload-slots stream
    ;; Situations:
    ;; - We have more than max-peer-frame-size data, and peer window is above it -> we can send a frame and go on
    ;; - We do not have enough data (full frame) yet - we wait for more data
    ;; - We have more than max-peer-frame-size, but window is too small -> we read a frame
    (let ((total-length (- (+ (or end (length sequence))
                              (length (get-output-buffer stream)))
                           start)))
      (loop
        for frame-size = (http2/core::get-max-peer-frame-size connection)
        while (and (>= total-length frame-size))
        do
           (wait-for-window-is-at-least-frame-size connection base-http2-stream)
           (setf start (send-data stream  sequence start
                                  (- frame-size (length output-buffer))))
           (decf total-length frame-size))
      (loop for idx from start to (1- end)
            do (vector-push (aref sequence idx) output-buffer)))))

(defclass http2-stream-with-input-stream ()
  ((payload-input-stream :accessor get-payload-input-stream :initarg :payload-input-stream)
   (charset              :accessor get-charset              :initarg :charset)
   (compression          :accessor get-compression          :initarg :compression))
  (:documentation "HTTP2 stream that passes all its DATA frames to PAYLOAD-INPUT-STREAM."))

(defclass payload-input-stream (payload-stream trivial-gray-streams:fundamental-binary-input-stream)
  ((index :accessor get-index :initarg :index)
   (data  :accessor get-data  :initarg :data
          :documentation
          "tlist of accepted frames"))
  (:default-initargs :index 0 :data (cons nil nil))
  (:documentation
   "Binary stream that reads data from the http stream.

It keeps data from last data frame in BUFFER, starting with INDEX."))

(defmethod initialize-instance :after ((stream payload-input-stream) &key base-http2-stream &allow-other-keys)
  (setf (get-payload-input-stream base-http2-stream) stream))

(defun empty-data-p (data)
  (null (car data)))

(defun push-frame (data frame)
  (let ((tail (cons frame nil)))
    (if (empty-data-p data)
        (setf (car data) tail)
        (setf (cddr data) tail))
    (setf (cdr data) tail))
  data)

(defun pop-frame (data)
  (pop (car data)))

(defmethod apply-data-frame ((stream http2-stream-with-input-stream) frame-data start end)
  (push-frame (get-data (get-payload-input-stream stream))
              (subseq  frame-data start end)))

(defmethod trivial-gray-streams:stream-listen ((stream payload-input-stream))
  (with-slots (base-http2-stream index data to-store to-provide) stream
    (with-slots (connection state) base-http2-stream
      (with-slots (network-stream) connection
        (force-output network-stream)
        (or (caar data)
            ;; we assume we would not get just part of the frame.
            (loop while (listen network-stream)
                  do (read-frame connection)
                  when (member state '(closed half-closed/remote))
                    do (return nil)
                  when (caar data) do (return t)))))))

(defmethod trivial-gray-streams:stream-read-byte ((stream payload-input-stream))
  (with-slots (base-http2-stream index data to-store to-provide) stream
    (with-slots (connection state) base-http2-stream
      (with-slots (network-stream) connection
          (force-output network-stream)
        (loop for buffer = (caar data)
              ;; read till either we have the buffer or stream is closed
              until (or buffer (member state '(closed half-closed/remote)))
              do
                 ;; read-frame -> push frame to the data or close the buffer
                 (if (listen network-stream)
                     (read-frame connection)
                     (sleep 0.1))
              finally
                 (return
                   (if (and (null buffer) (member state '(closed half-closed/remote))) :eof
                       (prog1 (aref buffer index)
                         (when (= (incf index) (length (caar data)))
                           (pop-frame data)
                           (write-window-update-frame connection index)
                           (write-window-update-frame base-http2-stream index)
                           (setf index 0))))))))))

(defmethod close ((stream payload-input-stream) &key &allow-other-keys)
  ;; do nothing
  )

(defvar *charset-names*
  '(("UTF-8" . :utf-8))
  "Translation table from header charset names to FLEXI-STREAM keywords.")

(defvar *default-encoding* nil
  "Character encoding to be used when not recognized from headers. Default is nil
- binary.")

(defvar *default-text-encoding* :utf8
  "Character encoding for text/ content to be used when not recognized from headers.")

(defun extract-charset-from-content-type (content-type)
  "Guess charset from the content type. NIL for binary data."
  (acond
    ((null content-type)
     (warn "No content type specified, using ~a" *default-encoding*)
     *default-encoding*)
    ((search #1="charset=" content-type)
     (let ((header-charset (subseq content-type (+ (length #1#) it))))
       (or (cdr (assoc header-charset *charset-names* :test 'string-equal))
           (warn "Unrecognized charset ~s, using default ~a" header-charset
                 *default-text-encoding*)
           *default-text-encoding*)))
    ((alexandria:starts-with-subseq "text/" content-type)
     (warn "Text without specified encoding, guessing utf-8")
     *default-text-encoding*)
    ((alexandria:starts-with-subseq "binary/" content-type) nil)
    ;; see RFC8259. Note that there should not be charset in json CT
    ((string-equal content-type "application/json") :utf-8)
    (t (warn "Content-type ~s not known to be text nor binary. Using default ~a"
             content-type *default-encoding*)
       *default-encoding*)))

(defun make-transport-output-stream-from-stream (raw-stream charset gzip)
  "An OUTPUT-STREAM built atop RAW STREAM with added charset and possibly
compression."
  (let* ((transport raw-stream))
    (when gzip
      (setf transport (gzip-stream:make-gzip-output-stream transport)))
    (awhen charset
      (setf transport
            (flexi-streams:make-flexi-stream
             transport
             :external-format it)))
    transport))

(defun make-transport-output-stream (raw-stream charset gzip)
  "An OUTPUT-STREAM built atop RAW STREAM with added text to binary encoding using
charset (as understood by flexi-streams) and possibly gzip compression."
  (make-transport-output-stream-from-stream
   (make-instance 'payload-output-stream :base-http2-stream raw-stream)
   charset gzip))

(defun make-transport-input-stream-from-stream (raw-stream charset encoded)
  "INPUT-STREAM built atop RAW-STREAM.

Guess encoding and need to gunzip from headers:
- apply zip decompression content-encoding is gzip (FIXME: also compression)
- use charset if understood in content-type
- otherwise guess whether text (use UTF-8) or binary."
  (let* ((transport raw-stream))
    (when encoded
      (setf transport (gzip-stream:make-gzip-input-stream transport)))
    (awhen charset
      (setf transport
            (flexi-streams:make-flexi-stream transport :external-format charset)))
    transport))

(defun make-transport-input-stream (raw-stream charset gzip)
  "INPUT-STREAM built atop RAW-STREAM.

Guess encoding and need to gunzip from headers:
- apply zip decompression if gzip is set
- if charset is not null, use it to convert to text."
  (make-transport-input-stream-from-stream
   (make-instance 'payload-input-stream :base-http2-stream raw-stream)
   charset gzip))
