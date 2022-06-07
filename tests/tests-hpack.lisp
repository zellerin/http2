(in-package http2)


(fiasco:deftest int-pack-tests ()

    ;;
  (fiasco:is (= 10 (get-integer-from-octet nil #x0a 5)) "
 C.1.1.  Example 1: Encoding 10 Using a 5-Bit Prefix

    The value 10 is to be encoded with a 5-bit prefix.

    o  10 is less than 31 (2^5 - 1) and is represented using the 5-bit
       prefix.

      0   1   2   3   4   5   6   7
    +---+---+---+---+---+---+---+---+
    | X | X | X | 0 | 1 | 0 | 1 | 0 |   10 stored on 5 bits
    +---+---+---+---+---+---+---+---+")
  ;;
  (let ((*bytes-left* 2))
    (fiasco:is
        (= 1337
           (multiple-value-bind (write read) (make-pipe)
             (write-sequence #(154 10) write)
             (get-integer-from-octet read #x1f 5)))
        "
     C.1.2.  Example 2: Encoding 1337 Using a 5-Bit Prefix

          0   1   2   3   4   5   6   7
        +---+---+---+---+---+---+---+---+
        | X | X | X | 1 | 1 | 1 | 1 | 1 |  Prefix = 31, I = 1306
        | 1 | 0 | 0 | 1 | 1 | 0 | 1 | 0 |  1306>=128, encode(154), I=1306/128
        | 0 | 0 | 0 | 0 | 1 | 0 | 1 | 0 |  10<128, encode(10), done
        +---+---+---+---+---+---+---+---+")

    (fiasco:is (zerop *bytes-left*) "C.1.2 read different bytes."))

  (fiasco:is (= 42 (get-integer-from-octet nil 42 8))
      "C.1.3.  Example 3: Encoding 42 Starting at an Octet Boundary

   The value 42 is to be encoded starting at an octet boundary.  This
   implies that a 8-bit prefix is used.

     0   1   2   3   4   5   6   7
   +---+---+---+---+---+---+---+---+
   | 0 | 0 | 1 | 0 | 1 | 0 | 1 | 0 |   42 stored on 8 bits
   +---+---+---+---+---+---+---+---+"
      ))
#|


C.2.  Header Field Representation Examples

   This section shows several independent representation examples.

C.2.1.  Literal Header Field with Indexing

   The header field representation uses a literal name and a literal
   value.  The header field is added to the dynamic table.

|#

(fiasco:deftest headers-representation ()
  (multiple-value-bind (write read) (make-pipe)
    (flet ((test-decode (source-text expect-name expect-value &optional dynamic-table)
             (let ((*bytes-read* 0)
                   (connection (make-instance 'http2-connection :network-stream read)))
                                        ; C.2.1
               (write-sequence (vector-from-hex-text source-text) write)
               (destructuring-bind (name value)
                   (print (read-http-header connection))
                 (fiasco:is (equal name expect-name))
                 (fiasco:is (equal value expect-value))
                                        ;    [  1] (s =  55) custom-key: custom-header
                                        ;          Table size:  55
                 (fiasco:is (equalp (if dynamic-table (vector `(,name ,value)) #())
                                    (get-dynamic-table connection)))))))
      (test-decode  "400a637573746f6d2d6b65790d637573746f6d2d686561646572"
                     "custom-key"  "custom-header" t)

      ;; C.2.2.  Literal Header Field without Indexing

      (test-decode "040c2f73616d706c652f70617468" :path  "/sample/path")

      ;; C.2.3.  Literal Header Field Never Indexed
      (test-decode "100870617373776f726406736563726574"
                   "password" "secret")

      ;; C.2.4 Indexed header field
      (test-decode "82" :method "GET"))))

#|

C.3.  Request Examples without Huffman Coding

   This section shows several consecutive header lists, corresponding to
   HTTP requests, on the same connection.

C.3.1.  First Request

   Header list to encode:

   :method: GET
   :scheme: http
   :path: /
   :authority: www.example.com

   Hex dump of encoded data:

   8286 8441 0f77 7777 2e65 7861 6d70 6c65 | ...A.www.example
   2e63 6f6d                               | .com








Peon & Ruellan               Standards Track                   [Page 37]


RFC 7541                          HPACK                         May 2015


   Decoding process:

   82                                      | == Indexed - Add ==
                                           |   idx = 2
                                           | -> :method: GET
   86                                      | == Indexed - Add ==
                                           |   idx = 6
                                           | -> :scheme: http
   84                                      | == Indexed - Add ==
                                           |   idx = 4
                                           | -> :path: /
   41                                      | == Literal indexed ==
                                           |   Indexed name (idx = 1)
                                           |     :authority
   0f                                      |   Literal value (len = 15)
   7777 772e 6578 616d 706c 652e 636f 6d   | www.example.com
                                           | -> :authority:
                                           |   www.example.com

   Dynamic Table (after decoding):

   [  1] (s =  57) :authority: www.example.com
         Table size:  57

   Decoded header list:

   :method: GET
   :scheme: http
   :path: /
   :authority: www.example.com

C.3.2.  Second Request

   Header list to encode:

   :method: GET
   :scheme: http
   :path: /
   :authority: www.example.com
   cache-control: no-cache

   Hex dump of encoded data:

   8286 84be 5808 6e6f 2d63 6163 6865      | ....X.no-cache







Peon & Ruellan               Standards Track                   [Page 38]


RFC 7541                          HPACK                         May 2015


   Decoding process:

   82                                      | == Indexed - Add ==
                                           |   idx = 2
                                           | -> :method: GET
   86                                      | == Indexed - Add ==
                                           |   idx = 6
                                           | -> :scheme: http
   84                                      | == Indexed - Add ==
                                           |   idx = 4
                                           | -> :path: /
   be                                      | == Indexed - Add ==
                                           |   idx = 62
                                           | -> :authority:
                                           |   www.example.com
   58                                      | == Literal indexed ==
                                           |   Indexed name (idx = 24)
                                           |     cache-control
   08                                      |   Literal value (len = 8)
   6e6f 2d63 6163 6865                     | no-cache
                                           | -> cache-control: no-cache

   Dynamic Table (after decoding):

   [  1] (s =  53) cache-control: no-cache
   [  2] (s =  57) :authority: www.example.com
         Table size: 110

   Decoded header list:

   :method: GET
   :scheme: http
   :path: /
   :authority: www.example.com
   cache-control: no-cache

C.3.3.  Third Request

   Header list to encode:

   :method: GET
   :scheme: https
   :path: /index.html
   :authority: www.example.com
   custom-key: custom-value






Peon & Ruellan               Standards Track                   [Page 39]


RFC 7541                          HPACK                         May 2015


   Hex dump of encoded data:

   8287 85bf 400a 6375 7374 6f6d 2d6b 6579 | ....@.custom-key
   0c63 7573 746f 6d2d 7661 6c75 65        | .custom-value

   Decoding process:

   82                                      | == Indexed - Add ==
                                           |   idx = 2
                                           | -> :method: GET
   87                                      | == Indexed - Add ==
                                           |   idx = 7
                                           | -> :scheme: https
   85                                      | == Indexed - Add ==
                                           |   idx = 5
                                           | -> :path: /index.html
   bf                                      | == Indexed - Add ==
                                           |   idx = 63
                                           | -> :authority:
                                           |   www.example.com
   40                                      | == Literal indexed ==
   0a                                      |   Literal name (len = 10)
   6375 7374 6f6d 2d6b 6579                | custom-key
   0c                                      |   Literal value (len = 12)
   6375 7374 6f6d 2d76 616c 7565           | custom-value
                                           | -> custom-key:
                                           |   custom-value

   Dynamic Table (after decoding):

   [  1] (s =  54) custom-key: custom-value
   [  2] (s =  53) cache-control: no-cache
   [  3] (s =  57) :authority: www.example.com
         Table size: 164

   Decoded header list:

   :method: GET
   :scheme: https
   :path: /index.html
   :authority: www.example.com
   custom-key: custom-value









Peon & Ruellan               Standards Track                   [Page 40]


RFC 7541                          HPACK                         May 2015


C.4.  Request Examples with Huffman Coding

   This section shows the same examples as the previous section but uses
   Huffman encoding for the literal values.

C.4.1.  First Request

   Header list to encode:

   :method: GET
   :scheme: http
   :path: /
   :authority: www.example.com

   Hex dump of encoded data:

   8286 8441 8cf1 e3c2 e5f2 3a6b a0ab 90f4 | ...A......:k....
   ff                                      | .

   Decoding process:

   82                                      | == Indexed - Add ==
                                           |   idx = 2
                                           | -> :method: GET
   86                                      | == Indexed - Add ==
                                           |   idx = 6
                                           | -> :scheme: http
   84                                      | == Indexed - Add ==
                                           |   idx = 4
                                           | -> :path: /
   41                                      | == Literal indexed ==
                                           |   Indexed name (idx = 1)
                                           |     :authority
   8c                                      |   Literal value (len = 12)
                                           |     Huffman encoded:
   f1e3 c2e5 f23a 6ba0 ab90 f4ff           | .....:k.....
                                           |     Decoded:
                                           | www.example.com
                                           | -> :authority:
                                           |   www.example.com

   Dynamic Table (after decoding):

   [  1] (s =  57) :authority: www.example.com
         Table size:  57






Peon & Ruellan               Standards Track                   [Page 41]


RFC 7541                          HPACK                         May 2015


   Decoded header list:

   :method: GET
   :scheme: http
   :path: /
   :authority: www.example.com

C.4.2.  Second Request

   Header list to encode:

   :method: GET
   :scheme: http
   :path: /
   :authority: www.example.com
   cache-control: no-cache

   Hex dump of encoded data:

   8286 84be 5886 a8eb 1064 9cbf           | ....X....d..

   Decoding process:

   82                                      | == Indexed - Add ==
                                           |   idx = 2
                                           | -> :method: GET
   86                                      | == Indexed - Add ==
                                           |   idx = 6
                                           | -> :scheme: http
   84                                      | == Indexed - Add ==
                                           |   idx = 4
                                           | -> :path: /
   be                                      | == Indexed - Add ==
                                           |   idx = 62
                                           | -> :authority:
                                           |   www.example.com
   58                                      | == Literal indexed ==
                                           |   Indexed name (idx = 24)
                                           |     cache-control
   86                                      |   Literal value (len = 6)
                                           |     Huffman encoded:
   a8eb 1064 9cbf                          | ...d..
                                           |     Decoded:
                                           | no-cache
                                           | -> cache-control: no-cache






Peon & Ruellan               Standards Track                   [Page 42]


RFC 7541                          HPACK                         May 2015


   Dynamic Table (after decoding):

   [  1] (s =  53) cache-control: no-cache
   [  2] (s =  57) :authority: www.example.com
         Table size: 110

   Decoded header list:

   :method: GET
   :scheme: http
   :path: /
   :authority: www.example.com
   cache-control: no-cache

C.4.3.  Third Request

   Header list to encode:

   :method: GET
   :scheme: https
   :path: /index.html
   :authority: www.example.com
   custom-key: custom-value

   Hex dump of encoded data:

   8287 85bf 4088 25a8 49e9 5ba9 7d7f 8925 | ....@.%.I.[.}..%
   a849 e95b b8e8 b4bf                     | .I.[....























Peon & Ruellan               Standards Track                   [Page 43]


RFC 7541                          HPACK                         May 2015


   Decoding process:

   82                                      | == Indexed - Add ==
                                           |   idx = 2
                                           | -> :method: GET
   87                                      | == Indexed - Add ==
                                           |   idx = 7
                                           | -> :scheme: https
   85                                      | == Indexed - Add ==
                                           |   idx = 5
                                           | -> :path: /index.html
   bf                                      | == Indexed - Add ==
                                           |   idx = 63
                                           | -> :authority:
                                           |   www.example.com
   40                                      | == Literal indexed ==
   88                                      |   Literal name (len = 8)
                                           |     Huffman encoded:
   25a8 49e9 5ba9 7d7f                     | %.I.[.}.
                                           |     Decoded:
                                           | custom-key
   89                                      |   Literal value (len = 9)
                                           |     Huffman encoded:
   25a8 49e9 5bb8 e8b4 bf                  | %.I.[....
                                           |     Decoded:
                                           | custom-value
                                           | -> custom-key:
                                           |   custom-value

   Dynamic Table (after decoding):

   [  1] (s =  54) custom-key: custom-value
   [  2] (s =  53) cache-control: no-cache
   [  3] (s =  57) :authority: www.example.com
         Table size: 164

   Decoded header list:

   :method: GET
   :scheme: https
   :path: /index.html
   :authority: www.example.com
   custom-key: custom-value








Peon & Ruellan               Standards Track                   [Page 44]


RFC 7541                          HPACK                         May 2015


C.5.  Response Examples without Huffman Coding

   This section shows several consecutive header lists, corresponding to
   HTTP responses, on the same connection.  The HTTP/2 setting parameter
   SETTINGS_HEADER_TABLE_SIZE is set to the value of 256 octets, causing
   some evictions to occur.

C.5.1.  First Response

   Header list to encode:

   :status: 302
   cache-control: private
   date: Mon, 21 Oct 2013 20:13:21 GMT
   location: https://www.example.com

   Hex dump of encoded data:

   4803 3330 3258 0770 7269 7661 7465 611d | H.302X.privatea.
   4d6f 6e2c 2032 3120 4f63 7420 3230 3133 | Mon, 21 Oct 2013
   2032 303a 3133 3a32 3120 474d 546e 1768 |  20:13:21 GMTn.h
   7474 7073 3a2f 2f77 7777 2e65 7861 6d70 | ttps://www.examp
   6c65 2e63 6f6d                          | le.com

   Decoding process:

   48                                      | == Literal indexed ==
                                           |   Indexed name (idx = 8)
                                           |     :status
   03                                      |   Literal value (len = 3)
   3330 32                                 | 302
                                           | -> :status: 302
   58                                      | == Literal indexed ==
                                           |   Indexed name (idx = 24)
                                           |     cache-control
   07                                      |   Literal value (len = 7)
   7072 6976 6174 65                       | private
                                           | -> cache-control: private
   61                                      | == Literal indexed ==
                                           |   Indexed name (idx = 33)
                                           |     date
   1d                                      |   Literal value (len = 29)
   4d6f 6e2c 2032 3120 4f63 7420 3230 3133 | Mon, 21 Oct 2013
   2032 303a 3133 3a32 3120 474d 54        |  20:13:21 GMT
                                           | -> date: Mon, 21 Oct 2013
                                           |   20:13:21 GMT
   6e                                      | == Literal indexed ==
                                           |   Indexed name (idx = 46)



Peon & Ruellan               Standards Track                   [Page 45]


RFC 7541                          HPACK                         May 2015


                                           |     location
   17                                      |   Literal value (len = 23)
   6874 7470 733a 2f2f 7777 772e 6578 616d | https://www.exam
   706c 652e 636f 6d                       | ple.com
                                           | -> location:
                                           |   https://www.example.com

   Dynamic Table (after decoding):

   [  1] (s =  63) location: https://www.example.com
   [  2] (s =  65) date: Mon, 21 Oct 2013 20:13:21 GMT
   [  3] (s =  52) cache-control: private
   [  4] (s =  42) :status: 302
         Table size: 222

   Decoded header list:

   :status: 302
   cache-control: private
   date: Mon, 21 Oct 2013 20:13:21 GMT
   location: https://www.example.com

C.5.2.  Second Response

   The (":status", "302") header field is evicted from the dynamic table
   to free space to allow adding the (":status", "307") header field.

   Header list to encode:

   :status: 307
   cache-control: private
   date: Mon, 21 Oct 2013 20:13:21 GMT
   location: https://www.example.com

   Hex dump of encoded data:

   4803 3330 37c1 c0bf                     | H.307...

   Decoding process:

   48                                      | == Literal indexed ==
                                           |   Indexed name (idx = 8)
                                           |     :status
   03                                      |   Literal value (len = 3)
   3330 37                                 | 307
                                           | - evict: :status: 302
                                           | -> :status: 307
   c1                                      | == Indexed - Add ==



Peon & Ruellan               Standards Track                   [Page 46]


RFC 7541                          HPACK                         May 2015


                                           |   idx = 65
                                           | -> cache-control: private
   c0                                      | == Indexed - Add ==
                                           |   idx = 64
                                           | -> date: Mon, 21 Oct 2013
                                           |   20:13:21 GMT
   bf                                      | == Indexed - Add ==
                                           |   idx = 63
                                           | -> location:
                                           |   https://www.example.com

   Dynamic Table (after decoding):

   [  1] (s =  42) :status: 307
   [  2] (s =  63) location: https://www.example.com
   [  3] (s =  65) date: Mon, 21 Oct 2013 20:13:21 GMT
   [  4] (s =  52) cache-control: private
         Table size: 222

   Decoded header list:

   :status: 307
   cache-control: private
   date: Mon, 21 Oct 2013 20:13:21 GMT
   location: https://www.example.com

C.5.3.  Third Response

   Several header fields are evicted from the dynamic table during the
   processing of this header list.

   Header list to encode:

   :status: 200
   cache-control: private
   date: Mon, 21 Oct 2013 20:13:22 GMT
   location: https://www.example.com
   content-encoding: gzip
   set-cookie: foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1












Peon & Ruellan               Standards Track                   [Page 47]


RFC 7541                          HPACK                         May 2015


   Hex dump of encoded data:

   88c1 611d 4d6f 6e2c 2032 3120 4f63 7420 | ..a.Mon, 21 Oct
   3230 3133 2032 303a 3133 3a32 3220 474d | 2013 20:13:22 GM
   54c0 5a04 677a 6970 7738 666f 6f3d 4153 | T.Z.gzipw8foo=AS
   444a 4b48 514b 425a 584f 5157 454f 5049 | DJKHQKBZXOQWEOPI
   5541 5851 5745 4f49 553b 206d 6178 2d61 | UAXQWEOIU; max-a
   6765 3d33 3630 303b 2076 6572 7369 6f6e | ge=3600; version
   3d31                                    | =1

   Decoding process:

   88                                      | == Indexed - Add ==
                                           |   idx = 8
                                           | -> :status: 200
   c1                                      | == Indexed - Add ==
                                           |   idx = 65
                                           | -> cache-control: private
   61                                      | == Literal indexed ==
                                           |   Indexed name (idx = 33)
                                           |     date
   1d                                      |   Literal value (len = 29)
   4d6f 6e2c 2032 3120 4f63 7420 3230 3133 | Mon, 21 Oct 2013
   2032 303a 3133 3a32 3220 474d 54        |  20:13:22 GMT
                                           | - evict: cache-control:
                                           |   private
                                           | -> date: Mon, 21 Oct 2013
                                           |   20:13:22 GMT
   c0                                      | == Indexed - Add ==
                                           |   idx = 64
                                           | -> location:
                                           |   https://www.example.com
   5a                                      | == Literal indexed ==
                                           |   Indexed name (idx = 26)
                                           |     content-encoding
   04                                      |   Literal value (len = 4)
   677a 6970                               | gzip
                                           | - evict: date: Mon, 21 Oct
                                           |    2013 20:13:21 GMT
                                           | -> content-encoding: gzip
   77                                      | == Literal indexed ==
                                           |   Indexed name (idx = 55)
                                           |     set-cookie
   38                                      |   Literal value (len = 56)
   666f 6f3d 4153 444a 4b48 514b 425a 584f | foo=ASDJKHQKBZXO
   5157 454f 5049 5541 5851 5745 4f49 553b | QWEOPIUAXQWEOIU;
   206d 6178 2d61 6765 3d33 3630 303b 2076 |  max-age=3600; v
   6572 7369 6f6e 3d31                     | ersion=1



Peon & Ruellan               Standards Track                   [Page 48]


RFC 7541                          HPACK                         May 2015


                                           | - evict: location:
                                           |   https://www.example.com
                                           | - evict: :status: 307
                                           | -> set-cookie: foo=ASDJKHQ
                                           |   KBZXOQWEOPIUAXQWEOIU; ma
                                           |   x-age=3600; version=1

   Dynamic Table (after decoding):

   [  1] (s =  98) set-cookie: foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU;
                    max-age=3600; version=1
   [  2] (s =  52) content-encoding: gzip
   [  3] (s =  65) date: Mon, 21 Oct 2013 20:13:22 GMT
         Table size: 215

   Decoded header list:

   :status: 200
   cache-control: private
   date: Mon, 21 Oct 2013 20:13:22 GMT
   location: https://www.example.com
   content-encoding: gzip
   set-cookie: foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1

C.6.  Response Examples with Huffman Coding

   This section shows the same examples as the previous section but uses
   Huffman encoding for the literal values.  The HTTP/2 setting
   parameter SETTINGS_HEADER_TABLE_SIZE is set to the value of 256
   octets, causing some evictions to occur.  The eviction mechanism uses
   the length of the decoded literal values, so the same evictions occur
   as in the previous section.

C.6.1.  First Response

   Header list to encode:

   :status: 302
   cache-control: private
   date: Mon, 21 Oct 2013 20:13:21 GMT
   location: https://www.example.com

   Hex dump of encoded data:

   4882 6402 5885 aec3 771a 4b61 96d0 7abe | H.d.X...w.Ka..z.
   9410 54d4 44a8 2005 9504 0b81 66e0 82a6 | ..T.D. .....f...
   2d1b ff6e 919d 29ad 1718 63c7 8f0b 97c8 |) -..n.....c.....
   e9ae 82ae 43d3                          | ....C.



Peon & Ruellan               Standards Track                   [Page 49]


RFC 7541                          HPACK                         May 2015


   Decoding process:

   48                                      | == Literal indexed ==
                                           |   Indexed name (idx = 8)
                                           |     :status
   82                                      |   Literal value (len = 2)
                                           |     Huffman encoded:
   6402                                    | d.
                                           |     Decoded:
                                           | 302
                                           | -> :status: 302
   58                                      | == Literal indexed ==
                                           |   Indexed name (idx = 24)
                                           |     cache-control
   85                                      |   Literal value (len = 5)
                                           |     Huffman encoded:
   aec3 771a 4b                            | ..w.K
                                           |     Decoded:
                                           | private
                                           | -> cache-control: private
   61                                      | == Literal indexed ==
                                           |   Indexed name (idx = 33)
                                           |     date
   96                                      |   Literal value (len = 22)
                                           |     Huffman encoded:
   d07a be94 1054 d444 a820 0595 040b 8166 | .z...T.D. .....f
   e082 a62d 1bff                          | ...-..
                                           |     Decoded:
                                           | Mon, 21 Oct 2013 20:13:21
                                           | GMT
                                           | -> date: Mon, 21 Oct 2013
                                           |   20:13:21 GMT
   6e                                      | == Literal indexed ==
                                           |   Indexed name (idx = 46)
                                           |     location
   91                                      |   Literal value (len = 17)
                                           |     Huffman encoded:
   9d29 ad17 1863 c78f 0b97 c8e9 ae82 ae43 | .)...c.........C
   d3                                      | .
                                           |     Decoded:
                                           | https://www.example.com
                                           | -> location:
                                           |   https://www.example.com








Peon & Ruellan               Standards Track                   [Page 50]


RFC 7541                          HPACK                         May 2015


   Dynamic Table (after decoding):

   [  1] (s =  63) location: https://www.example.com
   [  2] (s =  65) date: Mon, 21 Oct 2013 20:13:21 GMT
   [  3] (s =  52) cache-control: private
   [  4] (s =  42) :status: 302
         Table size: 222

   Decoded header list:

   :status: 302
   cache-control: private
   date: Mon, 21 Oct 2013 20:13:21 GMT
   location: https://www.example.com

C.6.2.  Second Response

   The (":status", "302") header field is evicted from the dynamic table
   to free space to allow adding the (":status", "307") header field.

   Header list to encode:

   :status: 307
   cache-control: private
   date: Mon, 21 Oct 2013 20:13:21 GMT
   location: https://www.example.com

   Hex dump of encoded data:

   4883 640e ffc1 c0bf                     | H.d.....

   Decoding process:

   48                                      | == Literal indexed ==
                                           |   Indexed name (idx = 8)
                                           |     :status
   83                                      |   Literal value (len = 3)
                                           |     Huffman encoded:
   640e ff                                 | d..
                                           |     Decoded:
                                           | 307
                                           | - evict: :status: 302
                                           | -> :status: 307
   c1                                      | == Indexed - Add ==
                                           |   idx = 65
                                           | -> cache-control: private
   c0                                      | == Indexed - Add ==
                                           |   idx = 64



Peon & Ruellan               Standards Track                   [Page 51]


RFC 7541                          HPACK                         May 2015


                                           | -> date: Mon, 21 Oct 2013
                                           |   20:13:21 GMT
   bf                                      | == Indexed - Add ==
                                           |   idx = 63
                                           | -> location:
                                           |   https://www.example.com

   Dynamic Table (after decoding):

   [  1] (s =  42) :status: 307
   [  2] (s =  63) location: https://www.example.com
   [  3] (s =  65) date: Mon, 21 Oct 2013 20:13:21 GMT
   [  4] (s =  52) cache-control: private
         Table size: 222

   Decoded header list:

   :status: 307
   cache-control: private
   date: Mon, 21 Oct 2013 20:13:21 GMT
   location: https://www.example.com

C.6.3.  Third Response

   Several header fields are evicted from the dynamic table during the
   processing of this header list.

   Header list to encode:

   :status: 200
   cache-control: private
   date: Mon, 21 Oct 2013 20:13:22 GMT
   location: https://www.example.com
   content-encoding: gzip
   set-cookie: foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1

   Hex dump of encoded data:

   88c1 6196 d07a be94 1054 d444 a820 0595 | ..a..z...T.D. ..
   040b 8166 e084 a62d 1bff c05a 839b d9ab | ...f...-...Z....
   77ad 94e7 821d d7f2 e6c7 b335 dfdf cd5b | w..........5...[
   3960 d5af 2708 7f36 72c1 ab27 0fb5 291f | 9`..'..6r..'..).
   9587 3160 65c0 03ed 4ee5 b106 3d50 07   | ..1`e...N...=P.








Peon & Ruellan               Standards Track                   [Page 52]


RFC 7541                          HPACK                         May 2015


   Decoding process:

   88                                      | == Indexed - Add ==
                                           |   idx = 8
                                           | -> :status: 200
   c1                                      | == Indexed - Add ==
                                           |   idx = 65
                                           | -> cache-control: private
   61                                      | == Literal indexed ==
                                           |   Indexed name (idx = 33)
                                           |     date
   96                                      |   Literal value (len = 22)
                                           |     Huffman encoded:
   d07a be94 1054 d444 a820 0595 040b 8166 | .z...T.D. .....f
   e084 a62d 1bff                          | ...-..
                                           |     Decoded:
                                           | Mon, 21 Oct 2013 20:13:22
                                           | GMT
                                           | - evict: cache-control:
                                           |   private
                                           | -> date: Mon, 21 Oct 2013
                                           |   20:13:22 GMT
   c0                                      | == Indexed - Add ==
                                           |   idx = 64
                                           | -> location:
                                           |   https://www.example.com
   5a                                      | == Literal indexed ==
                                           |   Indexed name (idx = 26)
                                           |     content-encoding
   83                                      |   Literal value (len = 3)
                                           |     Huffman encoded:
   9bd9 ab                                 | ...
                                           |     Decoded:
                                           | gzip
                                           | - evict: date: Mon, 21 Oct
                                           |    2013 20:13:21 GMT
                                           | -> content-encoding: gzip
   77                                      | == Literal indexed ==
                                           |   Indexed name (idx = 55)
                                           |     set-cookie
   ad                                      |   Literal value (len = 45)
                                           |     Huffman encoded:
   94e7 821d d7f2 e6c7 b335 dfdf cd5b 3960 | .........5...[9`
   d5af 2708 7f36 72c1 ab27 0fb5 291f 9587 | ..'..6r..'..)...
   3160 65c0 03ed 4ee5 b106 3d50 07        | 1`e...N...=P.
                                           |     Decoded:
                                           | foo=ASDJKHQKBZXOQWEOPIUAXQ
                                           | WEOIU; max-age=3600; versi



Peon & Ruellan               Standards Track                   [Page 53]


RFC 7541                          HPACK                         May 2015


                                           | on=1
                                           | - evict: location:
                                           |   https://www.example.com
                                           | - evict: :status: 307
                                           | -> set-cookie: foo=ASDJKHQ
                                           |   KBZXOQWEOPIUAXQWEOIU; ma
                                           |   x-age=3600; version=1

   Dynamic Table (after decoding):

   [  1] (s =  98) set-cookie: foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU;
                    max-age=3600; version=1
   [  2] (s =  52) content-encoding: gzip
   [  3] (s =  65) date: Mon, 21 Oct 2013 20:13:22 GMT
         Table size: 215

   Decoded header list:

   :status: 200
   cache-control: private
   date: Mon, 21 Oct 2013 20:13:22 GMT
   location: https://www.example.com
   content-encoding: gzip
   set-cookie: foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1
|#
