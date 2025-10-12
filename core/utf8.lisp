(in-package http2/core)

(defsection @utf8
    (:title "UTF-8 streams")
  (utf8-parser-mixin class)
  (fallback-all-is-ascii class))

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
  (let ((content-type (cdr (assoc "content-type" headers
                                  :test 'string-equal))))
    (anaphora:acond
      ((null content-type)
       (warn "No content type specified, using UTF-8")
       nil)
      ((search #1="charset=" content-type)
       (string-equal "UTF-8" (subseq content-type (+ (length #1#) it))))
      ((alexandria:starts-with-subseq "text/" content-type)
       (warn "Text without specified encoding, guessing utf-8")
       t)
      ;; see RFC8259. Note that there should not be charset in json CT
      ((string-equal content-type "application/json") t)
      (t nil))))

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
  "When headers satisfy IS-UTF8-P, convert the the binary frame to text (taking
care about UTF-8 characters possibly split between frames) and call
APPLY-TEXT-DATA-FRAME on it."
  (if (is-utf8-p (get-headers stream))
      (unless (= start end)  ; Google sends empty payload sometimes. Remove when
                                        ; we can handle that.
        (let* ((first-start-char-position
                 (position-if #'utf-is-first-char payload :start start :end end)))
          (when (null first-start-char-position)
            (error 'unimplemented-feature :format-control "FIXME: no start-char in payload ~s"
                                          :format-arguments (list  (subseq payload start end))))
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
             (trivial-utf-8:utf-8-bytes-to-string payload :start start :end  end)))))

      (call-next-method)))

(defclass fallback-all-is-ascii ()
  ()
  (:documentation "Treat all data input as ASCII, that is, convert octets to a string with
CODE-CHAR. It is compatible with UTF8-PARSER-MIXIN if provided after it in the
list of direct superclasses."))

(defmethod apply-data-frame ((stream fallback-all-is-ascii) payload start end)
  (http2/core::apply-text-data-frame stream (map 'string #'code-char (subseq payload start end))))
