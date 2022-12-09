;;;; Copyright 2022 by Tomáš Zellerin

(in-package :http2)

(fiasco:deftest int-pack-tests ()
  "Test that integer values are properly packed and unpacked"
  (fiasco:is (= 10 (get-integer-from-octet nil #x0a 5))
      "C.1.1.  Example 1: Encoding 10 Using a 5-Bit Prefix")
  (fiasco:is (equalp #(10) (integer-to-array 10 5 0))
      "C.1.1.  Example 1: Encoding 10 Using a 5-Bit Prefix")

  (let ((*bytes-left* 2))
    (fiasco:is
        (= 1337
           (multiple-value-bind (write read) (make-pipe)
             (write-sequence #(154 10) write)
             (get-integer-from-octet read #x1f 5)))
        "C.1.2.  Example 2: Encoding 1337 Using a 5-Bit Prefix")
    (fiasco:is (zerop *bytes-left*) "C.1.2 incorrect number of bytes"))
  (fiasco:is (equalp (integer-to-array 1337 5 0) #(31 154 10))
      "C.1.2 incorrect number of bytes")

  (fiasco:is (= 42 (get-integer-from-octet nil 42 8))
      "C.1.3.  Example 3: Encoding 42 Starting at an Octet Boundary"))

#|
C.2.  Header Field Representation Examples
|#

(fiasco:deftest headers-representation ()
  (multiple-value-bind (write read) (make-pipe)
    (flet ((test-decode (source-text expect-name expect-value &optional dynamic-table)
             (let* ((source (vector-from-hex-text source-text))
                    (*bytes-left* (length source))
                    (context (make-instance 'hpack-context)))
               (write-sequence source write)
               (destructuring-bind (name value)
                   (read-http-header read context)
                 (fiasco:is (equal name expect-name))
                 (fiasco:is (equal value expect-value))
                 (fiasco:is (equalp (if dynamic-table (vector `(,name ,value)) #())
                                    (get-dynamic-table context))))
               (fiasco:is (zerop *bytes-left*)))))
                                        ; C.2.1
      (test-decode  "400a637573746f6d2d6b65790d637573746f6d2d686561646572"
                     "custom-key"  "custom-header" t)

      ;; C.2.2.  Literal Header Field without Indexing

      (test-decode "040c2f73616d706c652f70617468" :path  "/sample/path")

      ;; C.2.3.  Literal Header Field Never Indexed
      (test-decode "100870617373776f726406736563726574"
                   "password" "secret")

      ;; C.2.4 Indexed header field
      (test-decode "82" :method "GET"))))

;; C.3.  Request Examples without Huffman Coding

(fiasco:deftest test-header-packing (connection encoded headers values)
  (fiasco:is (equalp (apply 'concatenate 'vector
                            (mapcar (lambda (a b)
                                      (encode-header a b
                                                     (get-compression-context connection)
                                                     nil))
                                    headers values))
                     (vector-from-hex-text encoded)))
  (fiasco:is
      (equalp (mapcar 'list headers values)
              (loop
                with source = (vector-from-hex-text encoded)
                initially (setf (get-buffer (get-network-stream connection)) source
                                (get-index (get-network-stream connection)) 0)

                with *bytes-left* = (length source)
         while (plusp *bytes-left*)
                collect (read-http-header (get-network-stream connection)
                                          (get-decompression-context connection))))))

(fiasco:deftest test-header-packings ()
  (let* ((connection (make-instance 'http2-connection
                                    :network-stream
                                    (make-instance 'pipe-end-for-read :index 0)))
         (decompression-context (get-decompression-context connection)))
    ;; C.3.1.  First Request
    (test-header-packing connection "828684410f7777772e6578616d706c652e636f6d"
                         '(:method :scheme :path :authority)
                         '("GET" "http" "/" "www.example.com"))
    (fiasco:is (equalp  (dynamic-table-value decompression-context 62) '(:AUTHORITY "www.example.com")))
    (fiasco:is (equalp  (get-dynamic-table decompression-context)
                        (get-dynamic-table (get-compression-context connection))))

    ;; C.3.2.  Second Request
    (test-header-packing connection "828684be58086e6f2d6361636865"
                            '(:method :scheme :path :authority "cache-control")
                            '("GET" "http" "/" "www.example.com" "no-cache"))
    (fiasco:is (equalp (dynamic-table-value decompression-context 62) '("cache-control" "no-cache")))
    (fiasco:is (equalp (dynamic-table-value decompression-context 63) '(:AUTHORITY "www.example.com")))
    (fiasco:is (equalp (get-dynamic-table (get-compression-context connection))
                       (get-dynamic-table decompression-context)))

    ;; C.3.3.  Third Request
    (test-header-packing connection "828785bf400a637573746f6d2d6b65790c637573746f6d2d76616c7565"
                         '(:method :scheme :path :authority "custom-key")
                         '("GET" "https" "/index.html" "www.example.com" "custom-value"))
    (fiasco:is (equalp  (dynamic-table-value decompression-context 62) '("custom-key" "custom-value")))
    (fiasco:is (equalp  (dynamic-table-value decompression-context 63) '("cache-control" "no-cache")))
    (fiasco:is (equalp  (dynamic-table-value decompression-context 64) '(:AUTHORITY "www.example.com")))
    (fiasco:is (equalp  (get-dynamic-table (get-compression-context connection)) (get-dynamic-table (get-decompression-context connection))))))

(fiasco:deftest test-header-decoder (stream decompression-context encoded headers values)
  (fiasco:is
      (equalp (mapcar 'list headers values)
              (loop
                with source = (vector-from-hex-text encoded)
                with *bytes-left* = (length source)
                for (name value) = (read-http-header
                                    stream decompression-context)
                initially (setf (get-buffer stream) source
                                (get-index stream) 0)



                collect (list name value)
                while (plusp *bytes-left*)))))

;; C.4.  Request Examples with Huffman Coding

(fiasco:deftest test-huffman-decoding ()
  (let* ((stream (make-instance 'pipe-end-for-read :index 0))
         (compression-context (make-instance 'hpack-context))
         (decompression-context (make-instance 'hpack-context)))

    ;; C.4.1.  First Request
    (test-huffman-header stream compression-context decompression-context
                         "828684418cf1e3c2e5f23a6ba0ab90f4ff"
                         '(:method :scheme :path :authority)
                         '("GET" "http" "/" "www.example.com"))
    (fiasco:is (equalp  (dynamic-table-value decompression-context 62) '(:authority "www.example.com")))
    ;; C.4.2.  Second Request
    (test-huffman-header stream compression-context decompression-context
                         "828684be5886a8eb10649cbf"
                         '(:method :scheme :path :authority "cache-control")
                         '("GET" "http" "/" "www.example.com" "no-cache"))
    (fiasco:is (equalp  (dynamic-table-value decompression-context 62) '("cache-control" "no-cache")))
    (fiasco:is (equalp  (dynamic-table-value decompression-context 63) '(:authority "www.example.com")))
    ;; C.4.3.  Third Request
    (test-huffman-header stream compression-context decompression-context
                         "828785bf408825a849e95ba97d7f8925a849e95bb8e8b4bf"
                         '(:method :scheme :path :authority "custom-key")
                         '("GET" "https" "/index.html" "www.example.com" "custom-value"))
    (fiasco:is (equalp  (dynamic-table-value decompression-context 62) '("custom-key" "custom-value")))
    (fiasco:is (equalp  (dynamic-table-value decompression-context 63) '("cache-control" "no-cache")))
    (fiasco:is (equalp  (dynamic-table-value decompression-context 64) '(:authority "www.example.com")))))

;; C.5.  Response Examples without Huffman Coding
(fiasco:deftest test-header-packings-response ()
  (let* ((connection (make-instance 'http2-connection
                                    :network-stream
                                    (make-instance 'pipe-end-for-read :index 0)
                                    :decompression-context (make-instance 'hpack-context :dynamic-table-size 256)))
         (decompression-context (get-decompression-context connection))
         (compression-context (get-compression-context connection)))
    ;; C.5.1.  First Response
    (test-header-packing connection "4803333032580770726976617465611d4d6f6e2c203231204f637420323031332032303a31333a323120474d546e1768747470733a2f2f7777772e6578616d706c652e636f6d"
                         '(:status "cache-control" "date" "location")
                         '("302" "private" "Mon, 21 Oct 2013 20:13:21 GMT" "https://www.example.com"))
    (fiasco:is (equalp (dynamic-table-value decompression-context 62) '("location" "https://www.example.com")))
    (fiasco:is (equalp (dynamic-table-value decompression-context 63) '("date" "Mon, 21 Oct 2013 20:13:21 GMT")))
    (fiasco:is (equalp (dynamic-table-value decompression-context 64) '("cache-control" "private")))
    (fiasco:is (equalp (dynamic-table-value decompression-context 65) '(:status "302")))
    (fiasco:is (equalp (get-bytes-left-in-table decompression-context) (- 256 222)))
    (fiasco:is (= (- (length (get-dynamic-table decompression-context))
                          (get-deleted-items decompression-context))
                  4))
    (fiasco:is (equalp (get-dynamic-table (get-compression-context connection)) (get-dynamic-table (get-decompression-context connection))))
    ;; C.5.2.  Second Response
    (test-header-packing connection "4803333037c1c0bf"
                         '(:status "cache-control" "date" "location")
                         '("307" "private" "Mon, 21 Oct 2013 20:13:21 GMT" "https://www.example.com"))
    (fiasco:is (equalp (dynamic-table-value decompression-context 63) '("location" "https://www.example.com")))
    (fiasco:is (equalp (dynamic-table-value decompression-context 64) '("date" "Mon, 21 Oct 2013 20:13:21 GMT")))
    (fiasco:is (equalp (dynamic-table-value decompression-context 65) '("cache-control" "private")))
    (fiasco:is (equalp (dynamic-table-value decompression-context 62) '(:status "307")))
    (fiasco:is (= (- (length (get-dynamic-table decompression-context))
                          (get-deleted-items decompression-context))
                  4))
    (fiasco:is (equalp (get-bytes-left-in-table decompression-context) (- 256 222)))

    ;; C.5.3.  Third Response
    (test-header-packing connection "88c1611d4d6f6e2c203231204f637420323031332032303a31333a323220474d54c05a04677a69707738666f6f3d4153444a4b48514b425a584f5157454f50495541585157454f49553b206d61782d6167653d333630303b2076657273696f6e3d31"
                         '(:status "cache-control" "date" "location" "content-encoding" "set-cookie")
                         '("200" "private" "Mon, 21 Oct 2013 20:13:22 GMT" "https://www.example.com" "gzip" "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1"))
    (fiasco:is (= (- (length (get-dynamic-table decompression-context))
                          (get-deleted-items decompression-context))
                  3))
    (fiasco:is (equalp (get-bytes-left-in-table decompression-context) (- 256 215)))
    (update-dynamic-table-size compression-context 256)
    (fiasco:is (equalp (get-bytes-left-in-table compression-context) (- 256 215)))))


;; C.6.  Response Examples with Huffman Coding
(defun test-huffman-header (stream compression-context decompression-context
                            encoded headers values)
  (test-header-decoder stream decompression-context encoded headers values)
  (fiasco:is (equalp (apply 'concatenate 'vector
                            (mapcar (lambda (a b)
                                      (encode-header a b
                                                     compression-context
                                                     t))
                                    headers values))
                     (vector-from-hex-text encoded))))

(fiasco:deftest test-header-packing-huffman-response ()
  (let* ((stream (make-instance 'pipe-end-for-read :index 0))
         (compression-context (make-instance 'hpack-context))
         (decompression-context (make-instance 'hpack-context)))
    ;; C.6.1.  First Response
    (test-huffman-header stream compression-context decompression-context
                         "488264025885aec3771a4b6196d07abe941054d444a8200595040b8166e082a62d1bff6e919d29ad171863c78f0b97c8e9ae82ae43d3"
                         '(:status "cache-control" "date" "location")
                         '("302" "private" "Mon, 21 Oct 2013 20:13:21 GMT" "https://www.example.com"))
    (fiasco:is (equalp  (dynamic-table-value decompression-context 62) '("location" "https://www.example.com")))
    (fiasco:is (equalp  (dynamic-table-value decompression-context 63) '("date" "Mon, 21 Oct 2013 20:13:21 GMT")))
    (fiasco:is (equalp  (dynamic-table-value decompression-context 64) '("cache-control" "private")))
    (fiasco:is (equalp  (dynamic-table-value decompression-context 65) '(:status "302")))

    ;; C.6.2.  Second Response
    (test-huffman-header stream compression-context decompression-context "4883640effc1c0bf"
                         '(:status "cache-control" "date" "location")
                         '("307" "private" "Mon, 21 Oct 2013 20:13:21 GMT" "https://www.example.com"))
    ;; here we should evict part of cache to fit 256 size, but we do not.
    ;; regardless, it works, which probably mean we need another tests.

    ;; C.6.3.  Third Response
    (test-huffman-header stream compression-context decompression-context
                         "88c16196d07abe941054d444a8200595040b8166e084a62d1bffc05a839bd9ab77ad94e7821dd7f2e6c7b335dfdfcd5b3960d5af27087f3672c1ab270fb5291f9587316065c003ed4ee5b1063d5007"
                         '(:status "cache-control" "date" "location" "content-encoding" "set-cookie")
                         '("200" "private" "Mon, 21 Oct 2013 20:13:22 GMT" "https://www.example.com" "gzip" "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1"))))
