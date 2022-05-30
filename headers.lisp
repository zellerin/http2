(in-package http2)

;;;; See RFC7541
(defvar static-headers-table
  (vector
   nil
   :authority
   '(:method "GET")
   '(:method "POST")
   '(:path "/")
   '(:path "/index.html")
   '(:scheme "http")
   '(:scheme "https")
   '(:status 200)
   '(:status 204)
   '(:status 206)
   '(:status 304)
   '(:status 400)
   '(:status 404)
   '(:status 500)
   "accept-charset"
   '("accept-encoding" "gzip, deflate")
   "accept-language"
   "accept-ranges"
   "accept"
   "access-control-allow-origin"
   "age"
   "allow"
   "authorization"
   "cache-control"
   "content-disposition"
   "content-encoding"
   "content-language"
   "content-length"
   "content-location"
   "content-range"
   "content-type"
   "cookie"
   "date"
   "etag"
   "expect"
   "expires"
   "from"
   "host"
   "if-match"
   "if-modified-since"
   "if-none-match"
   "if-range"
   "if-unmodified-since"
   "last-modified"
   "link"
   "location"
   "max-forwards"
   "proxy-authenticate"
   "proxy-authorization"
   "range"
   "referer"
   "refresh"
   "retry-after"
   "server"
   "set-cookie"
   "strict-transport-security"
   "transfer-encoding"
   "user-agent"
   "vary"
   "via"
   "www-authenticate"))

(defun encode-header (name value)
  (let ((res (make-array 0 :fill-pointer 0 :adjustable t)))
    (flet ((add-string (string)
             (loop for char across string
                   do (vector-push-extend (char-code char) res))))
      (let ((pos (position (list name value) static-headers-table :test 'equalp
                                                                  :end 17)))
        (when pos
          (vector-push-extend (logior #x80 pos) res)
          (return-from encode-header res)))
      (let ((header-pos (position name static-headers-table
                                  :test 'equal
                                  :key (lambda (a) (if (consp a) (car a) a)))))
        (when header-pos
          (vector-push-extend (logior #x40 header-pos) res)
          (vector-push-extend (length value) res)
          (add-string value)
          (return-from encode-header res)))
      ;; literal header field with new name
      (vector-push-extend #x40 res)
      (vector-push-extend (length name) res)
      (add-string name)
      (vector-push-extend (length value) res)
      (add-string value)
      res)))

(defun request-headers (method path authority &key (scheme "https"))
  (list (encode-header :method method)
        (encode-header :scheme scheme)
        (encode-header :path path)
        (encode-header :authority authority)))

(defvar *bytes-read* nil "Number of bytes read from stream")
(defun read-byte* (stream)
  (incf *bytes-read*)
  (read-byte stream))

(defun get-integer-from-octet (stream octet bit-size)
  "See 5.1 "
  (let ((small-res (ldb (byte bit-size 0) octet)))
    (when (= small-res (ldb (byte bit-size 0) -1))
      (loop
        with res = 0
        for word = (read-byte* stream)
        for shift from  0 by 7
        do
           (setf (ldb (byte 7 shift) res) (ldb (byte 7 0) word))
        when (zerop (ldb (byte 1 7) word))
          do (return-from get-integer-from-octet (+ res small-res)))
      ;; and do not forgot to increment bytes read
      )
    small-res))

(defun read-hufman (stream len)
  (loop with res = (make-array len :element-type '(unsigned-byte 8))
        for i from 0 to (1- len)
        do (setf (aref res i) (read-byte* stream))
        finally (return res)))

(defun read-string-from-stream (stream)
  (let* ((octet0 (read-byte* stream))
         (len (ldb (byte 7 0) octet0)))
    (if (plusp (ldb (byte 1 7) octet0))
        (read-hufman stream (get-integer-from-octet stream octet0 7))
        (loop with res = (make-array (get-integer-from-octet stream octet0 7)
                                     :element-type 'character)
              for i from 0 to (1- len)
              do  (setf (aref res i) (code-char (read-byte* stream)))))))

(defun read-from-tables (index connection)
  (cond ((zerop index)
         (error "Decoding error"))
        ((<= index (length static-headers-table))
         (aref static-headers-table index))
        (t (aref (get-dynamic-table connection)
                 (- index (length static-headers-table))))))

(defun read-http-header (connection)
  "Read header field from stream. Increment *bytes-read* as needed."
  (let* ((stream (get-network-stream connection))
         (octet0 (read-byte* stream)))
    (cond
      ((plusp (ldb (byte 1 7) octet0))
       (return-from read-http-header
         ;; 6.1 - indexed header
         (read-from-tables (ldb (byte 7 0) octet0) connection)))
      ((zerop (ldb (byte 6 0) octet0)) ; 0, 0x40
       ;; 6.2.1 - literal header, new name, with indexing
       (let ((header (read-string-from-stream stream)))
         (when (plusp octet0) (vector-push-extend header (get-dynamic-table connection)))
         (return-from read-http-header (list header  (read-string-from-stream stream)))))
      ((plusp (ldb (byte 1 6) octet0))
       ;; 6.2.1
       (let ((table-match (read-from-tables
                           (get-integer-from-octet stream octet0 6)
                           connection)))
         (list (if (consp table-match) (car table-match) table-match)
               (read-string-from-stream stream))))
      #+nil    (when (plusp (ldb (byte 1 6) octet0))
                 (let ((table-match (read-from-tables
                                     (get-integer-from-octet stream octet0 6)
                                     connection)))
                   (list (if (consp table-match) (car table-match) table-match)
                         (read-string-from-stream stream))))
      ((zerop (ldb (byte 4 4) octet0))
       (let ((table-match (read-from-tables
                           (get-integer-from-octet stream octet0 4)
                           connection)))
         (list (if (consp table-match) (car table-match) table-match)
               (read-string-from-stream stream))))
      (t (error "Fix code ~d" octet0)))))



(eval-when (:compile-toplevel :load-toplevel)
(defparameter *huffman-code*
  (vector '(#x1ff8 13)
   '(#x7fffd8 23)
   '(#xfffffe2 28)
   '(#xfffffe3 28)
   '(#xfffffe4 28)
   '(#xfffffe5 28)
   '(#xfffffe6 28)
   '(#xfffffe7 28)
   '(#xfffffe8 28)
   '(#xffffea 24)
   '(#x3ffffffc 30)
   '(#xfffffe9 28)
   '(#xfffffea 28)
    '(#x3ffffffd 30)
    '(#xfffffeb 28)
    '(#xfffffec 28)
    '(#xfffffed 28)
    '(#xfffffee 28)
    '(#xfffffef 28)
    '(#xffffff0 28)
    '(#xffffff1 28)
    '(#xffffff2 28)
    '(#x3ffffffe 30)
    '(#xffffff3 28)
    '(#xffffff4 28)
    '(#xffffff5 28)
    '(#xffffff6 28)
    '(#xffffff7 28)
    '(#xffffff8 28)
    '(#xffffff9 28)
    '(#xffffffa 28)
    '(#xffffffb 28)
    '(#x14  6)
    '(#x3f8 10)
    '(#x3f9 10)
    '(#xffa 12)
    '(#x1ff9 13)
    '(#x15  6)
    '(#xf8  8)
    '(#x7fa 11)
    '(#x3fa 10)
    '(#x3fb 10)
    '(#xf9  8)
    '(#x7fb 11)
    '(#xfa  8)
    '(#x16  6)
    '(#x17  6)
    '(#x18  6)
    '(#x0  5)
    '(#x1  5)
    '(#x2  5)
    '(#x19  6)
    '(#x1a  6)
    '(#x1b  6)
    '(#x1c  6)
    '(#x1d  6)
    '(#x1e  6)
    '(#x1f  6)
    '(#x5c  7)
    '(#xfb  8)
    '(#x7ffc 15)
    '(#x20  6)
    '(#xffb 12)
    '(#x3fc 10)
    '(#x1ffa 13)
    '(#x21  6)
    '(#x5d  7)
    '(#x5e  7)
    '(#x5f  7)
    '(#x60  7)
    '(#x61  7)
    '(#x62  7)
    '(#x63  7)
    '(#x64  7)
    '(#x65  7)
    '(#x66  7)
    '(#x67  7)
    '(#x68  7)
    '(#x69  7)
    '(#x6a  7)
    '(#x6b  7)
    '(#x6c  7)
    '(#x6d  7)
    '(#x6e  7)
    '(#x6f  7)
    '(#x70  7)
    '(#x71  7)
    '(#x72  7)
    '(#xfc  8)
    '(#x73  7)
    '(#xfd  8)
    '(#x1ffb 13)
    '(#x7fff0 19)
    '(#x1ffc 13)
    '(#x3ffc 14)
    '(#x22  6)
    '(#x7ffd 15)
    '(#x3  5)
    '(#x23  6)
    '(#x4  5)
    '(#x24  6)
    '(#x5  5)
    '(#x25  6)
    '(#x26  6)
    '(#x27  6)
    '(#x6  5)
    '(#x74  7)
    '(#x75  7)
    '(#x28  6)
    '(#x29  6)
    '(#x2a  6)
    '(#x7  5)
    '(#x2b  6)
    '(#x76  7)
    '(#x2c  6)
    '(#x8  5)
    '(#x9  5)
    '(#x2d  6)
    '(#x77  7)
    '(#x78  7)
    '(#x79  7)
    '(#x7a  7)
    '(#x7b  7)
    '(#x7ffe 15)
    '(#x7fc 11)
    '(#x3ffd 14)
    '(#x1ffd 13)
    '(#xffffffc 28)
    '(#xfffe6 20)
    '(#x3fffd2 22)
    '(#xfffe7 20)
    '(#xfffe8 20)
    '(#x3fffd3 22)
    '(#x3fffd4 22)
    '(#x3fffd5 22)
    '(#x7fffd9 23)
    '(#x3fffd6 22)
    '(#x7fffda 23)
    '(#x7fffdb 23)
    '(#x7fffdc 23)
    '(#x7fffdd 23)
    '(#x7fffde 23)
    '(#xffffeb 24)
    '(#x7fffdf 23)
    '(#xffffec 24)
    '(#xffffed 24)
    '(#x3fffd7 22)
    '(#x7fffe0 23)
    '(#xffffee 24)
    '(#x7fffe1 23)
    '(#x7fffe2 23)
    '(#x7fffe3 23)
    '(#x7fffe4 23)
    '(#x1fffdc 21)
    '(#x3fffd8 22)
    '(#x7fffe5 23)
    '(#x3fffd9 22)
    '(#x7fffe6 23)
    '(#x7fffe7 23)
    '(#xffffef 24)
    '(#x3fffda 22)
    '(#x1fffdd 21)
    '(#xfffe9 20)
    '(#x3fffdb 22)
    '(#x3fffdc 22)
    '(#x7fffe8 23)
    '(#x7fffe9 23)
    '(#x1fffde 21)
    '(#x7fffea 23)
    '(#x3fffdd 22)
    '(#x3fffde 22)
    '(#xfffff0 24)
    '(#x1fffdf 21)
    '(#x3fffdf 22)
    '(#x7fffeb 23)
    '(#x7fffec 23)
    '(#x1fffe0 21)
    '(#x1fffe1 21)
    '(#x3fffe0 22)
    '(#x1fffe2 21)
    '(#x7fffed 23)
    '(#x3fffe1 22)
    '(#x7fffee 23)
    '(#x7fffef 23)
    '(#xfffea 20)
    '(#x3fffe2 22)
    '(#x3fffe3 22)
    '(#x3fffe4 22)
    '(#x7ffff0 23)
    '(#x3fffe5 22)
    '(#x3fffe6 22)
    '(#x7ffff1 23)
    '(#x3ffffe0 26)
    '(#x3ffffe1 26)
    '(#xfffeb 20)
    '(#x7fff1 19)
    '(#x3fffe7 22)
    '(#x7ffff2 23)
    '(#x3fffe8 22)
    '(#x1ffffec 25)
    '(#x3ffffe2 26)
    '(#x3ffffe3 26)
    '(#x3ffffe4 26)
    '(#x7ffffde 27)
    '(#x7ffffdf 27)
    '(#x3ffffe5 26)
    '(#xfffff1 24)
    '(#x1ffffed 25)
    '(#x7fff2 19)
    '(#x1fffe3 21)
    '(#x3ffffe6 26)
    '(#x7ffffe0 27)
    '(#x7ffffe1 27)
    '(#x3ffffe7 26)
    '(#x7ffffe2 27)
    '(#xfffff2 24)
    '(#x1fffe4 21)
    '(#x1fffe5 21)
    '(#x3ffffe8 26)
    '(#x3ffffe9 26)
    '(#xffffffd 28)
    '(#x7ffffe3 27)
    '(#x7ffffe4 27)
    '(#x7ffffe5 27)
    '(#xfffec 20)
    '(#xfffff3 24)
    '(#xfffed 20)
    '(#x1fffe6 21)
    '(#x3fffe9 22)
    '(#x1fffe7 21)
    '(#x1fffe8 21)
    '(#x7ffff3 23)
    '(#x3fffea 22)
    '(#x3fffeb 22)
    '(#x1ffffee 25)
    '(#x1ffffef 25)
    '(#xfffff4 24)
    '(#xfffff5 24)
    '(#x3ffffea 26)
    '(#x7ffff4 23)
    '(#x3ffffeb 26)
    '(#x7ffffe6 27)
    '(#x3ffffec 26)
    '(#x3ffffed 26)
    '(#x7ffffe7 27)
    '(#x7ffffe8 27)
    '(#x7ffffe9 27)
    '(#x7ffffea 27)
    '(#x7ffffeb 27)
    '(#xffffffe 28)
    '(#x7ffffec 27)
    '(#x7ffffed 27)
    '(#x7ffffee 27)
    '(#x7ffffef 27)
    '(#x7fffff0 27)
    '(#x3ffffee 26)
    '(#x3fffffff 30)))

  (defun make-cond-branch (i prefix code)
    "Return decoder code for I matching PREFIX amonf codes."
    (let ((data (remove i code :key (lambda (a) (ldb (byte prefix (- (second a) prefix)) (first a)))
                               :test-not '=)))
      (cond ((cdr data)
             (decode-octet-fn
              (mapcar (lambda (a) (list (ldb (byte (- (second a) prefix) 0) (car a)) (- (second a) prefix) (third a))) data)))
            ((= (third (car data)) 256)
             '(return-from decode-huffman-to-stream))
            (t
             `(emit ,(code-char (third (car data))))))))

  (defun decode-octet-fn (&optional (codes
                                     (loop for i from 0 for c across *huffman-code*
                                           collect (append c (list i)))))
    (let ((min-prefix (min 8 (reduce 'min codes :key 'second))))
      `(progn
         (update-vars ,min-prefix)
         (case prefix
           ,@(loop for idx from 0 to (1- (expt 2 min-prefix))
                   collect `(,idx ,(make-cond-branch idx min-prefix codes))))))))


(defun decode-huffman-to-stream  (char-stream bytes &optional (nr 0) (nr-size 0) (prefix 0))
  (let ((idx 0))
    (declare ((unsigned-byte 16) nr)
             ((integer 0 35) nr-size prefix)
             (optimize (debug 3) speed)
             ((integer 0 65536) idx)
             ((and vector (simple-array (unsigned-byte 8))) bytes))
    (macrolet ((decode ()
                 (decode-octet-fn)))
      (flet ((update-vars (min-prefix)
               (when (> min-prefix nr-size)
                 (when (= idx (length bytes)) (return-from decode-huffman-to-stream))
                 (let ((old nr))
                   (setf nr 0
                         (ldb (byte 8 8) nr) old
                         (ldb (byte 8 0) nr) (aref bytes idx))
                   (incf nr-size 8)
                   (incf idx)))
               (setf prefix (ldb (byte min-prefix (- nr-size min-prefix)) nr))
               (decf nr-size min-prefix)
               (setf nr (ldb (byte nr-size 0) nr)))
             (emit (char) (write-char char char-stream)))
        (declare (optimize speed (safety 0) space)
                 (notinline update-vars emit))
        (loop (decode))))))

(defun decode-huffman (bytes)
  (with-output-to-string (s)
    (decode-huffman-to-stream s bytes)))
