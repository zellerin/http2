(in-package http2/core)

(defgeneric handle-alt-svc (stream origin value)
  (:documentation
   "An ALTSVC frame from a server to a client on a stream other than
   stream 0 indicates that the conveyed alternative service is
   associated with the origin of that stream.

   An ALTSVC frame from a server to a client on stream 0 indicates that
   the conveyed alternative service is associated with the origin
   contained in the Origin field of the frame.  An association with an
   origin that the client does not consider authoritative for the
   current connection MUST be ignored.")

  (:method (stream origin value)
    "Default method ignores alt-svc info."
    (declare (ignore stream origin value))
    (warn 'unimplemented-feature :format-control "alt-svc frames are not handled.")))

(define-frame-type 10 :altsvc-frame
    "See RFC 7838.  The ALTSVC HTTP/2 frame advertises the availability of an
   alternative service to an HTTP/2 client.

```
    +-------------------------------+-------------------------------+
    |         Origin-Len (16)       | Origin? (*)                 ...
    +-------------------------------+-------------------------------+
    |                   Alt-Svc-Field-Value (*)                   ...
    +---------------------------------------------------------------+
```"
    ((origin (or null string))
     (alt-svc-field-value string))
    (:length (+ 2 (length origin) (length alt-svc-field-value))
     :may-have-connection t)

    (lambda (buffer start origin alt-svc-field-value)
      (setf (aref/wide buffer start 2) (length origin))
      (when origin
        (replace buffer
                 (map '(vector unsigned-byte 8)
                      'char-code origin)
                 :start1 (+ start 2)))
      (replace buffer
               (map '(vector unsigned-byte 8)
                    'char-code alt-svc-field-value)
               :start1 (+ 2 start (length origin))))

    ;; reader
    (lambda (connection data http-stream flags)
      "Parse ALT-SVC frame and invoke HANDLE-ALT-SVC callback."
      (unless (zerop flags) (warn "Flags set for altsvc frame: ~d" flags))
      (let* ((origin-len (aref/wide data 0 2))
             (alt-svc-field-value (subseq data (+ 2 origin-len))))
        (cond
          ((and (eq connection http-stream)
                (plusp origin-len))
           (handle-alt-svc connection
                           (when (plusp origin-len)
                             (subseq data 2 (+ 2 origin-len)))
                           alt-svc-field-value))
          ((plusp origin-len)
           "An ALTSVC frame on a stream other than stream 0 containing non-empty \"Origin\"
   information is invalid and MUST be ignored.")
          ((eq connection http-stream)
           "An ALTSVC frame on stream 0 with empty (length 0) \"Origin\" information is
   invalid and MUST be ignored.")
          (t (handle-alt-svc http-stream nil alt-svc-field-value))))
      (values #'parse-frame-header 9)))
