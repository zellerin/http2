(in-package http2/stream-overlay)

(mgl-pax:defsection @stream-based-connection
    (:title "Connections using CL streams")
  (stream-based-connection-mixin class)
  (get-scheduler generic-function)
  (get-lock generic-function)
  (process-pending-frames function))

#+unused
(defun write-frame-header (stream length type flags http-stream R)
  "Write a frame header to STREAM."
  (write-sequence
   (write-frame-header-to-vector
    (make-octet-buffer 9) 0 length type flags (get-stream-id http-stream) R)
   stream))

(defun process-pending-frames (connection &optional just-pending (initial-action #'parse-frame-header) (initial-size 9))
  "@FRAME-HANDLER built atop CL streams.

Read and process frames on the input stream taken from the CONNECTION's network-stream.

Finish normally when either

- peer closes connection (END-OF-FILE, CONNECTION-ERROR condition or CL+SSL::SSL-ERROR was signalled), or
- JUST-PENDING was true, we are at a frame border and there is no additional input on the stream

This is to be called on client when the initial request was send, or on server
to serve requests.

May block."
  (declare (stream-based-connection-mixin connection))
  (handler-case
      (loop
        with frame-action = initial-action
        and size = initial-size
        and stream = (get-network-stream connection)
                  ;; Prevent ending when waiting for payload
        while (or (null just-pending)
                  (listen stream)
                  (not (eql #'parse-frame-header frame-action)))
        do
           (force-output stream)
           (let ((buffer (make-octet-buffer size)))
             (declare (dynamic-extent buffer))
             (if (= size (read-sequence buffer stream))
                 (multiple-value-setq
                     (frame-action size)
                   (funcall frame-action connection buffer))
                 (error 'end-of-file :stream (get-network-stream connection)))
             #-debug (when (null size) (error "Bad handler for ~a" buffer))))
    (cl+ssl::ssl-error ()
      ;; peer may close connection and strange things happen
      (error 'end-of-file :stream (get-network-stream connection)))
    (connection-error (ce)
      (format t "-> We close connection due to ~a~%" ce)
      (invoke-restart 'http2/server::close-connection))))

(defun read-frame (connection &optional (network-stream (get-network-stream connection)))
  "Read one frame related to the CONNECTION from STREAM. Flush outstanding data to
write, read the header and process it."
  (declare (inline make-octet-buffer)
           (stream-based-connection-mixin))
  (force-output network-stream)
  (let ((buffer (make-octet-buffer 9)))
    (declare (dynamic-extent buffer))
    (when (< (read-sequence buffer network-stream) 9)
      (error 'end-of-file :stream connection))
    (multiple-value-bind (receive-fn length)
        (parse-frame-header connection buffer)
      (declare (compiled-function receive-fn)
               ((unsigned-byte 24) length))
      (loop while (not (equal #'parse-frame-header receive-fn))
            do
               (let* ((frame-content (make-octet-buffer length))
                      (read (read-sequence frame-content network-stream)))
                 (when (< read length)
                   (error 'end-of-file :stream connection))
                 (multiple-value-setq (receive-fn length)
                   (funcall receive-fn connection frame-content))))
      (force-output network-stream))))
