(in-package http2)

(defun trace-frame-printer (depth fn-name keyword stack-frame values)
  "Function to be called on write-frame-header tracing"
  (declare (ignore stack-frame depth))
  (assert (eql fn-name 'write-frame-header) ()
          "This should be callback on write-frame-header")
  (case keyword
    (:enter
     (destructuring-bind (stream length type flags http-stream R) values
       (declare (ignore stream R))
       (let ((frame-type (aref *frame-types* type)))
         (format *trace-output* "~&~s send ~s (#x~:x octets) flags ~{~a~^ ~}~%"
                 http-stream frame-type length
                 (if (plusp flags)
                     (loop for flag-name in (frame-type-flag-keywords frame-type)
                           when (get-flag flags flag-name)
                             collect flag-name)
                     '("None"))))))))

(defun trace-http2 (&key all (frames all) (settings all))
  (cond
    (frames
     (trace read-frame)
     (trace write-frame-header
            . #+sbcl (:report trace-frame-printer)))
    (t (untrace read-frame write-frame-header)))
  (cond
    (settings
     (trace set-peer-setting))
    (t (untrace set-peer-setting))))
