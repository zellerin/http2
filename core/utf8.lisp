(in-package http2)

(defun utf-is-first-char (octet)
  (not (= (logand octet #xc0) #x80)))

(defun utf-first-char-size (octet)
  (cond
    ((= #x00 (logand octet #x80)) 1)
    ((= #xc0 (logand octet #xe0)) 2)
    ((= #xe0 (logand octet #xf0)) 3)
    ((= #xf0 (logand octet #xf8)) 4)
    ;; 20240607 TODO: Better UTF error
    (t (error "UTF error"))))

(defun is-utf8-p (headers)
  "Test headers to see if the encoding is UTF-8."
  (eq :utf-8 (extract-charset-from-content-type
              (cdr (assoc "content-type" headers
                          :test 'string-equal)))))

(defgeneric apply-text-data-frame (stream text))

(defclass utf8-parser-mixin ()
  ((broken-char :accessor get-broken-char :initarg :broken-char :initform nil))
  (:documentation
   "Defines a method on APPLY-DATA-FRAME that, if CONTENT-TYPE indicates UTF-8,
reads octets, converts them to UTF-8 text, and calls APPLY-TEXT-DATA-FRAME on
the stream and the text."))

(defmethod apply-data-frame ((stream utf8-parser-mixin) payload start end)
  ;; Handle correctly broken (into pieces) multi-octet letters
  ;;
  ;; 1) find first start char, and if it is not in the
  ;; beginning, add all before it to the broken-char
  ;; this might be slow but rare enough
  (if (is-utf8-p (get-headers stream))
      (let* ((first-start-char-position
               (position-if #'utf-is-first-char payload :start start :end end)))
        (when (null first-start-char-position)
          (error "FIXME: no start-char in buffer"))
        (unless (= first-start-char-position start)
          ;; FIXME: print broken char
          (assert (get-broken-char stream))
          (apply-text-data-frame
           stream
           (trivial-utf-8:utf-8-bytes-to-string
            (concatenate '(vector (unsigned-byte 8)) (get-broken-char stream)
                         (subseq payload start first-start-char-position))))
          (setf start first-start-char-position))
        ;; 2) find whether last starting char is fully in the buffer
        (let* ((last-start-char-position
                 (position-if #'utf-is-first-char payload :from-end t
                                                          :start start
                                                          :end end))
               (last-start-char (aref payload last-start-char-position)))
          (unless (= (+ last-start-char-position (utf-first-char-size last-start-char))
                     end)
            (setf (get-broken-char stream) (subseq payload last-start-char-position end)
                  end last-start-char-position))
          (apply-text-data-frame
           stream
           (trivial-utf-8:utf-8-bytes-to-string payload :start start :end  end))))
      (call-next-method)))
