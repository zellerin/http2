;;;; Copyright 2022 by Tomáš Zellerin

(in-package :http2/hpack)

;;;; See RFC7541
(defvar static-headers-table
  (vector
   nil
   :authority
   '(:method "GET") '(:method "POST") '(:path "/") '(:path "/index.html")
   '(:scheme "http") '(:scheme "https") '(:status "200") '(:status "204")
   '(:status "206") '(:status "304") '(:status "400") '(:status "404")
   '(:status "500") "accept-charset" '("accept-encoding" "gzip, deflate")
   "accept-language" "accept-ranges" "accept" "access-control-allow-origin"
   "age" "allow" "authorization" "cache-control" "content-disposition"
   "content-encoding" "content-language" "content-length" "content-location"
   "content-range" "content-type" "cookie" "date" "etag" "expect" "expires"
   "from" "host" "if-match" "if-modified-since" "if-none-match" "if-range"
   "if-unmodified-since" "last-modified" "link" "location" "max-forwards"
   "proxy-authenticate" "proxy-authorization" "range" "referer" "refresh"
   "retry-after" "server" "set-cookie" "strict-transport-security"
   "transfer-encoding" "user-agent" "vary" "via" "www-authenticate")

  "Static headers table. Each element is either list of header name and value, or
just header name. The content is defined in the RFC7541, and is supposed to
start at index 1, so leading nil.")

(defconstant +last-static-header-index+ 61)
(defconstant +last-static-header-pair+ 17)

(defclass hpack-context ()
  ((dynamic-table       :accessor get-dynamic-table        :initarg :dynamic-table)
   (bytes-left-in-table :accessor get-bytes-left-in-table
                        :initarg :dynamic-table-size)
   (dynamic-table-size  :accessor get-dynamic-table-size
                        :initarg :dynamic-table-size)
   (updates-needed      :accessor get-updates-needed       :initarg :updates-needed)
   (deleted-items       :accessor get-deleted-items        :initarg :deleted-items))
  (:default-initargs
   :dynamic-table (make-array 0 :fill-pointer 0 :adjustable t)
   ;; When a connection is established, the dynamic table size for the HPACK
   ;; decoder and encoder at both endpoints starts at 4,096 bytes, the initial
   ;; value of the SETTINGS_HEADER_TABLE_SIZE setting.
   :dynamic-table-size 4096
   :deleted-items 0
   :updates-needed nil)

  (:documentation
   "Dynamic tables implementation: they are stored in an adjustable array, with
   [0] element storing first element initially (indexed by s+1), and (s+k)th element after k
   insertions.

   After deletion of D elements, element s+k is stored on index D, element s+1
   on index

     <----------  Index Address Space ---------->
     <-- Static  Table -->  <--   Dynamic Table -->
     +---+-----------+---+  +-----+-----------+-----+
     | 1 |    ...    | s |  |s+1+D|    ...    |s+k+D| ...deleted...
     +---+-----------+---+  +-----+-----------+-----+
                               k                D                 0
                            <----- table aref index -------------->
                            ^                   |
                            |                   V
                     Insertion Point      Dropping Point"))

(declaim
 (ftype (function (vector fixnum) fixnum) vector-index-to-hpack-index)
 (ftype (function (hpack-context fixnum) (or cons string)) dynamic-table-value))

(deftype context-table-element ()
  '(or cons string))

(defun vector-index-to-hpack-index (table idx)
  "Convert between hpack index and index in the vector. This works both ways."
  (- (+ +last-static-header-index+ (length table)) idx))

(defun dynamic-table-value (context idx)
  "Header on position IDX in the dynamic table in CONTEXT.

This is factored out to be used in testing."
  (aref (get-dynamic-table context)
        (vector-index-to-hpack-index (get-dynamic-table context) idx)))

(defun find-in-tables (context item test key static-end)
  "Find something (pair or header name) in static and possibly dynamic table."
  ;; there are several clear possible improvements there; however, if you want
  ;; performance so much, better precompile the headers in compile time.
  (or (position item static-headers-table :test test
                                          :end static-end
                                          :key key)
      (when context
        (let ((dyn (position item (get-dynamic-table context)
                             :start (get-deleted-items context)
                             :test test
                             :key key)))
          (if dyn (vector-index-to-hpack-index (get-dynamic-table context) dyn))))))

(defun find-pair-in-tables (context pair)
  "Find header PAIR in static table and, if CONNECTION is not null, in its dynamic
table. Return the index, or NIL if not found."
  ;; 17 is last pair in static table
  (find-in-tables context pair #'equalp #'identity +last-static-header-pair+))

(defun find-header-in-tables (context name)
  "Find header NAME in static table and, if CONNECTION is not null, in its dynamic
table. Return the index, or NIL if not found."
  (find-in-tables context name #'equal
                  (lambda (a) (if (consp a) (car a) a))
                  nil))

(defun compute-header-size (header)
  "Size of the header for dynamic cache purposes: 32 octets plus header and name
sizes, including leading : at special header names."
  (let* ((name (first header))
         (keyword (keywordp name)))
    (+ (if keyword 33 32)
       (length (if keyword (symbol-name name) name))
       (length (second header)))))

(defun update-dynamic-table-size (context new-size)
  "Update dynamic table for new size that is smaller than the previous one.

Zero size means evict completely; in this case the new vector can be cleaned"
  (cond ((zerop new-size)
         (setf (fill-pointer (get-dynamic-table context)) new-size)
         (adjust-array (get-dynamic-table context) new-size))
        (t
         (loop with table = (get-dynamic-table context)
               with bytes-left = new-size
               for idx from (1- (length table)) downto (get-deleted-items context)
               for this-header-size = (compute-header-size (aref table idx))
               if (>= bytes-left this-header-size)
                 do (decf bytes-left this-header-size)
               else
                 do
                    ;; evict earlier
                    (loop for i from (get-deleted-items context) to (1+ idx)
                          do (setf (aref table idx) nil))
                    (setf (get-deleted-items context) (1+ idx)
                          (get-bytes-left-in-table context) bytes-left)
                    (return)))))

(defmethod add-dynamic-header (context header)
  "Add dynamic header to a table. Return the header."
  (vector-push-extend header (get-dynamic-table context))
  (decf (get-bytes-left-in-table context)
        (compute-header-size header))
  ;; flush out records till we fit the space.
  (loop with table = (get-dynamic-table context)
        while (minusp (get-bytes-left-in-table context))
        for to-evict =  (aref table (get-deleted-items context))
        do
           (incf (get-bytes-left-in-table context)
                 (compute-header-size to-evict))
           (setf (aref table (get-deleted-items context)) nil)
           (incf (get-deleted-items context)))
  header)

(defvar *use-huffman-coding-by-default* :maybe
  "Is set, the headers are by default huffman-encoded. Special value :maybe means
whatever is shorter, plain encoding in case of draw.")

(defun store-string (string res huffman)
  "Add STRING to fillable array RES in format defined by HPACK, possibly using
huffman encoding."
  (let ((huffman-size (and huffman (huffman-coded-size string)))
        (string-size (length string)))
    (cond
      ((and huffman (or (> string-size huffman-size)
                        (not (eq huffman :maybe))))
       (write-integer-to-array res
                               huffman-size
                               7
                               128)
       (encode-huffman string res))
      (t
       (loop
         initially (write-integer-to-array res string-size 7 0)
         for char across string
             do (vector-push-extend (char-code char) res))))))

(defconstant +literal-header-noindex+ #x0)
(defconstant +literal-header-index+ #x40)
(defconstant +literal-header-never-index+ #x10)

(defun write-literal-header-pair (res code name value huffman)
  (vector-push-extend code res)
  (flet ((add-string (string)
           (store-string string res huffman)))
    (add-string name)
    (add-string value)))

(defun write-indexed-header-pair (res index)
  (write-integer-to-array res index 7 #x80))

(defun write-indexed-name (res code index value use-bits huffman)
  (write-integer-to-array res index use-bits code)
  (store-string value res huffman))

(defun header-writer (res name value &optional context (huffman *use-huffman-coding-by-default*))
  "Encode header consisting of NAME and VALUE.

The `never-indexed` format is never generated, use a separate function for
this (and this function needs to be written).

When CONTEXT is provided, use incremental indexing with dynamic table in
that CONTEXT.

Use Huffman when HUFFMAN is true."
  ;; note: ECL reports error in the declaration w/o type
  (declare (type (or null hpack-context) context))
  (acond
    ((find-pair-in-tables context (list name value))
     (write-indexed-header-pair res it))
    ((find-header-in-tables context name)
     (cond
       (context
        (write-indexed-name res +literal-header-index+ it value 6 huffman)
        (add-dynamic-header context (list name value)))
       (t (write-indexed-name res +literal-header-noindex+ it value 4 huffman))))
    (context
     (write-literal-header-pair res +literal-header-index+ name value huffman)
     (add-dynamic-header context (list name value)))
    (t
     (write-literal-header-pair res +literal-header-noindex+ name value huffman)))
  res)

(defun encode-dynamic-table-update (res new-size)
  "Encode table update to NEW-SIZE to an adjustable array RES."
  (write-integer-to-array res new-size 5 #x20)
  res)

(defun encode-header (name value &optional context (huffman *use-huffman-coding-by-default*))
  "Encode header consisting of NAME and VALUE.

The `never-indexed` format is never generated,
use a separate function for this (and this function needs to be written).

When CONTEXT is provided, use incremental indexing with dynamic table in
that CONTEXT.

Use Huffman when HUFFMAN is true."
  ;; note: ECL reports error in the declaration w/o type
  (declare (type (or null hpack-context) context))
  (let ((res (make-array 0 :fill-pointer 0 :adjustable t)))
    (header-writer res name value context huffman)))

(defun compute-update-dynamic-size-codes (res updates)
  (when updates
    ;; we need to send update to both minimum and then (if different) final
    ;; size.
    (let ((min-update (reduce #'min updates))
          (last-update (car updates)))
      (encode-dynamic-table-update res min-update)
      (when (< min-update last-update)
        (encode-dynamic-table-update res last-update))
      res)))

(defun compile-headers (headers context)
  (loop
    with res = (make-array 0 :fill-pointer 0 :adjustable t)
    initially (when context
                (compute-update-dynamic-size-codes
                 res (get-updates-needed context)))
    for header in headers
    do (if (vectorp header)
           (loop for octet across header
                 do (vector-push-extend octet res))
           (header-writer res (car header)
                          (second header)
                          context))
    finally (return (list res))))

(defun request-headers (method path authority
                        &key (scheme "https")
                          content-type
                          gzip-content
                          additional-headers)
  "Encode standard request headers that are obligatory."
  (compile-headers
   `((:method, (if (symbolp method) (symbol-name method) method))
     (:scheme ,scheme)
     (:path ,(or path "/"))
     (:authority ,authority)
     ,@(when content-type
           `(("content-type" ,content-type)))
     ,@(when gzip-content
         '(("content-encoding" "gzip")))
     ,@(mapcar (lambda (a)
                 (list (car a) (cdr a)))
               additional-headers))
   nil))

(defun get-integer-from-octet (stream initial-octet bit-size)
  "Decode an integer from starting OCTET and additional octets in STREAM
as defined in RFC7541 sect. 5.1."
  (declare (type (integer 1 8) bit-size)
           (type (unsigned-byte 8) initial-octet))
  (let ((small-res (ldb (byte bit-size 0) initial-octet)))
    (if (= small-res (ldb (byte bit-size 0) -1))
        (loop
          with res fixnum = 0
          for octet of-type (unsigned-byte 8) = (read-byte* stream)
          for shift from 0 by 7
          for last-octet = (zerop (ldb (byte 1 7) octet))
          and octet-value = (ldb (byte 7 0) octet)
          do (setf (ldb (byte 7 shift) res) octet-value)
          when last-octet
            do (return (+ res small-res)))
        small-res)))

(defun write-integer-to-array (array integer bit-size mask)
  "Write integer to a fillable vector as defined in RFC7541 sect. 5.1.

Return the fillable vector."
  (cond
    ((> (1- (expt 2 bit-size)) integer)
     (vector-push-extend (logior integer mask) array))
    (t
     (vector-push-extend (logior mask (1- (expt 2 bit-size))) array)
     (decf integer (1- (expt 2 bit-size)))
     (loop while (>= integer 128)
           do
              (vector-push-extend (logior 128 (ldb (byte 7 0) integer)) array)
              (setf integer (floor integer 128))
           finally
              (vector-push-extend integer array))))
  array)

(defun integer-to-array (integer bit-size mask)
  "Represent integer to a vector as defined in RFC7541 sect. 5.1."
  (let ((array (make-array 0 :adjustable t :fill-pointer 0)))
    (write-integer-to-array array integer bit-size mask)
    array))

(defun read-huffman (stream len)
  "Read Huffman coded text of length LEN from STREAM."
  (loop with res = (make-array len :element-type '(unsigned-byte 8))
        for i from 0 to (1- len)
        do (setf (aref res i) (read-byte* stream))
        finally (return (decode-huffman res))))

(defun read-string-from-stream (stream)
  "Read string literal from a STREAM as defined in RFC7541 sect 5.2. "
  (let* ((octet0 (read-byte* stream))
         (len (ldb (byte 7 0) octet0))
         (size (get-integer-from-octet stream octet0 7)))
    (if (plusp (ldb (byte 1 7) octet0))
        (read-huffman stream size)
        (loop with res = (make-array size
                                     :element-type 'character)
              for i from 0 to (1- len)
              do  (setf (aref res i) (code-char (read-byte* stream)))
              finally (return res)))))


(defun read-from-tables (index context)
  "Read item on INDEX in static table or dynamic table in CONNECTION."
  (cond ((zerop index)
         (error "Decoding error"))
        ((< index (length static-headers-table))
         (aref static-headers-table index))
        (t (dynamic-table-value context index))))

(defun read-literal-header-indexed-name (stream octet0 context use-bits)
  "See Fig. 6 and Fig. 8 - Name of header is in tables, value is literal.

Use USE-BITS from the OCTET0 for name index"
  (let ((table-match (read-from-tables
                      (get-integer-from-octet stream octet0 use-bits)
                      context)))
    (list (if (consp table-match) (car table-match) table-match)
          (read-string-from-stream stream))))

(defun read-literal-header-field-new-name (stream)
  "See 6.2.1 fig. 7, and 6.2.2. fig. 9 -
Neither name of the header nor value is in table, so read both as literals."
  (list (read-string-from-stream stream) (read-string-from-stream stream)))

(defun read-http-header (stream context)
  "Read header field from network stream associated with the CONNECTION."
  (let* ((octet0 (read-byte* stream)))
    (cond
      ((plusp (ldb (byte 1 7) octet0))  ;; 1xxx xxxx
       (read-from-tables (get-integer-from-octet stream octet0 7) context))
      ((= #x40 octet0) ;; 0100 0000 - fig. 7
       (add-dynamic-header context (read-literal-header-field-new-name stream)))
      ((plusp (ldb (byte 1 6) octet0)) ;; 01NN NNNN
       (add-dynamic-header context
                           (read-literal-header-indexed-name stream octet0 context 6)))
      ((zerop (logand #xef octet0)) ;; 000x 0000 - fig. 9 and 11
       (read-literal-header-field-new-name stream))
      ((zerop (ldb (byte 3 5) octet0))  ;; 000X NNNN
       (read-literal-header-indexed-name stream octet0 context 4))
      (t  ; 001x xxxx
       (update-dynamic-table-size context
                                  (get-integer-from-octet stream octet0 5))
       nil))))



(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *huffman-code*
    #((#x1ff8 13) (#x7fffd8 23) (#xfffffe2 28) (#xfffffe3 28) (#xfffffe4 28) (#xfffffe5 28)
      (#xfffffe6 28) (#xfffffe7 28) (#xfffffe8 28) (#xffffea 24) (#x3ffffffc 30) (#xfffffe9 28)
      (#xfffffea 28) (#x3ffffffd 30) (#xfffffeb 28) (#xfffffec 28) (#xfffffed 28) (#xfffffee 28)
      (#xfffffef 28) (#xffffff0 28) (#xffffff1 28) (#xffffff2 28) (#x3ffffffe 30) (#xffffff3 28)
      (#xffffff4 28) (#xffffff5 28) (#xffffff6 28) (#xffffff7 28) (#xffffff8 28) (#xffffff9 28)
      (#xffffffa 28) (#xffffffb 28) (#x14  6) (#x3f8 10) (#x3f9 10) (#xffa 12) (#x1ff9 13) (#x15 6)
      (#xf8 8) (#x7fa 11) (#x3fa 10) (#x3fb 10) (#xf9 8) (#x7fb 11) (#xfa 8) (#x16 6) (#x17 6) (#x18 6)
      (#x0 5) (#x1 5) (#x2 5) (#x19 6) (#x1a 6) (#x1b 6) (#x1c 6) (#x1d 6) (#x1e 6) (#x1f 6) (#x5c 7)
      (#xfb 8) (#x7ffc 15) (#x20 6) (#xffb 12) (#x3fc 10) (#x1ffa 13) (#x21 6) (#x5d 7) (#x5e 7)
      (#x5f 7) (#x60 7) (#x61 7) (#x62 7) (#x63 7) (#x64 7) (#x65 7) (#x66 7) (#x67 7) (#x68 7) (#x69 7)
      (#x6a 7) (#x6b 7) (#x6c 7) (#x6d 7) (#x6e 7) (#x6f 7) (#x70 7) (#x71 7) (#x72 7) (#xfc 8) (#x73 7)
      (#xfd 8) (#x1ffb 13) (#x7fff0 19) (#x1ffc 13) (#x3ffc 14) (#x22 6) (#x7ffd 15) (#x3 5) (#x23 6)
      (#x4 5) (#x24 6) (#x5 5) (#x25 6) (#x26 6) (#x27 6) (#x6 5) (#x74 7) (#x75 7) (#x28 6) (#x29 6)
      (#x2a 6) (#x7 5) (#x2b 6) (#x76 7) (#x2c 6) (#x8 5) (#x9 5) (#x2d 6) (#x77 7) (#x78 7) (#x79 7)
      (#x7a 7) (#x7b 7) (#x7ffe 15) (#x7fc 11) (#x3ffd 14) (#x1ffd 13) (#xffffffc 28) (#xfffe6 20)
      (#x3fffd2 22) (#xfffe7 20) (#xfffe8 20) (#x3fffd3 22) (#x3fffd4 22) (#x3fffd5 22) (#x7fffd9 23)
      (#x3fffd6 22) (#x7fffda 23) (#x7fffdb 23) (#x7fffdc 23) (#x7fffdd 23) (#x7fffde 23) (#xffffeb 24)
      (#x7fffdf 23) (#xffffec 24) (#xffffed 24) (#x3fffd7 22) (#x7fffe0 23) (#xffffee 24) (#x7fffe1 23)
      (#x7fffe2 23) (#x7fffe3 23) (#x7fffe4 23) (#x1fffdc 21) (#x3fffd8 22) (#x7fffe5 23) (#x3fffd9 22)
      (#x7fffe6 23) (#x7fffe7 23) (#xffffef 24) (#x3fffda 22) (#x1fffdd 21) (#xfffe9 20) (#x3fffdb 22)
      (#x3fffdc 22) (#x7fffe8 23) (#x7fffe9 23) (#x1fffde 21) (#x7fffea 23) (#x3fffdd 22) (#x3fffde 22)
      (#xfffff0 24) (#x1fffdf 21) (#x3fffdf 22) (#x7fffeb 23) (#x7fffec 23) (#x1fffe0 21) (#x1fffe1 21)
      (#x3fffe0 22) (#x1fffe2 21) (#x7fffed 23) (#x3fffe1 22) (#x7fffee 23) (#x7fffef 23) (#xfffea 20)
      (#x3fffe2 22) (#x3fffe3 22) (#x3fffe4 22) (#x7ffff0 23) (#x3fffe5 22) (#x3fffe6 22) (#x7ffff1 23)
      (#x3ffffe0 26) (#x3ffffe1 26) (#xfffeb 20) (#x7fff1 19) (#x3fffe7 22) (#x7ffff2 23) (#x3fffe8 22)
      (#x1ffffec 25) (#x3ffffe2 26) (#x3ffffe3 26) (#x3ffffe4 26) (#x7ffffde 27) (#x7ffffdf 27)
      (#x3ffffe5 26) (#xfffff1 24) (#x1ffffed 25) (#x7fff2 19) (#x1fffe3 21) (#x3ffffe6 26) (#x7ffffe0 27)
      (#x7ffffe1 27) (#x3ffffe7 26) (#x7ffffe2 27) (#xfffff2 24) (#x1fffe4 21) (#x1fffe5 21) (#x3ffffe8 26)
      (#x3ffffe9 26) (#xffffffd 28) (#x7ffffe3 27) (#x7ffffe4 27) (#x7ffffe5 27) (#xfffec 20) (#xfffff3 24)
      (#xfffed 20) (#x1fffe6 21) (#x3fffe9 22) (#x1fffe7 21) (#x1fffe8 21) (#x7ffff3 23) (#x3fffea 22)
      (#x3fffeb 22) (#x1ffffee 25) (#x1ffffef 25) (#xfffff4 24) (#xfffff5 24) (#x3ffffea 26) (#x7ffff4 23)
      (#x3ffffeb 26) (#x7ffffe6 27) (#x3ffffec 26) (#x3ffffed 26) (#x7ffffe7 27) (#x7ffffe8 27)
      (#x7ffffe9 27) (#x7ffffea 27) (#x7ffffeb 27) (#xffffffe 28) (#x7ffffec 27) (#x7ffffed 27)
      (#x7ffffee 27) (#x7ffffef 27) (#x7fffff0 27) (#x3ffffee 26) (#x3fffffff 30))
    "The code-and-code-size table from the RFC 7541 (hpack) appendix B")

  (defun make-cond-branch (i prefix code)
    "Return decoder code for I matching PREFIX among codes."
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

(defun encode-huffman (string res)
  "Convert string to huffman encoding."
  (loop
    with free-bits = 8
    with this-byte = 0
    for char across string
    for (code len) = (aref *huffman-code* (char-code char))
    do
       (loop
         for bits-in-this-byte = (min free-bits len)
         while (plusp len)
         do
            (setf (ldb (byte bits-in-this-byte (- free-bits bits-in-this-byte)) this-byte)
                  (ldb (byte bits-in-this-byte (- len bits-in-this-byte)) code))
            (decf free-bits bits-in-this-byte)
            (decf len bits-in-this-byte)
         when (zerop free-bits)
           do
              (vector-push-extend this-byte res)
              (setf this-byte 0 free-bits 8))
    finally
       (unless (= free-bits 8)
         (setf (ldb (byte free-bits 0) this-byte) -1)
         (vector-push-extend this-byte res))
       (return res)))

(defun huffman-coded-size (string)
  "Size of huffman-coded text"
  (loop
    for char across string
    for (code len) = (aref *huffman-code* (char-code char))
    sum len into total-len
    finally (return (ceiling total-len 8))))
