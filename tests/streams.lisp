(in-package :http2)

(fiasco:deftest guess-encodings-from-content-type ()
  "Test encoding guesses based on the content-type."
  (let ((*default-encoding* :foo)
        (*default-text-encoding* :bar))
    (handler-bind ((warning 'muffle-warning))
      (fiasco:is (equalp (extract-charset-from-content-type "text/plain; charset=uTf-8")
                         :utf-8))
      (fiasco:is (equalp (extract-charset-from-content-type "text/plain; charset=latIn-2")
                         :bar))
      (fiasco:is (equalp (extract-charset-from-content-type "text/plain; charset=utf-8")
                         :utf-8))
      (fiasco:is (equalp (extract-charset-from-content-type "text/plain; charset=latin-2")
                         :bar))
      (fiasco:is (equalp (extract-charset-from-content-type "application/binary")
                         :foo))
      (fiasco:is (null (extract-charset-from-content-type "binary/whatever")))
      (fiasco:is (equalp (extract-charset-from-content-type nil)
                         :foo))
      (fiasco:is (equalp (extract-charset-from-content-type "text/plain")
                         :bar))
      (fiasco:is (equalp (extract-charset-from-content-type "application/json")
                         :utf-8)))))

(fiasco:deftest out-stream-test ()
  "Write content of different type to possibly gzipped stream."
  (flet ((@ (text charset encode)
             (multiple-value-bind (out in) (make-pipe)
               (let ((s (make-transport-output-stream-from-stream out charset encode)))
                 (write-sequence text s)
                 (close s)
                 (get-buffer in)))))

    ;; text
    (fiasco:is (equalp (@ "Žába" :utf-8 nil)
                       #(197 189 195 161 98 97)))

    (fiasco:is (equalp (@ "Žába" :utf-8 t)
                       #(31 139 8 0 0 0 0 0 4
                         3 59 186 247 240 194
                         164 68 0 85 204 140
                         201 6 0 0 0)))

    ;; binary data
    (fiasco:is (equalp (@ #(1 2 3 4) nil nil)
                       #(1 2 3 4)))

    (fiasco:is (equalp (@ #(1 2 3 4) nil t)
                       #(31 139 8 0 0 0 0 0 4
                         3 99 100 98 102 1 0
                         205 251 60 182 4 0 0 0)))))

(fiasco:deftest in-stream-test ()
  "Reading UTF8 from possibly gzipped stream"
  (flet ((@ (data charset compressed)
             (multiple-value-bind (out in) (make-pipe)
               (write-sequence data out)
               (close out)
               (make-transport-input-stream-from-stream in charset compressed))))

    ;; expected usage - text
    (fiasco:is (equalp (read-line (@ #(197 189 195 161 98 97)
                                      :utf-8 nil))
                       "Žába"))

    (fiasco:is (equalp (read-line (@
                                   #(31 139 8 0 0 0 0 0 4
                                     3 59 186 247 240 194
                                     164 68 0 85 204 140
                                     201 6 0 0 0)
                                    :utf-8 t))
                       "Žába"))

    (let ((res-array (make-octet-buffer 4)))
      (read-sequence res-array  (@ #(1 2 3 4) nil nil))
      (fiasco:is (equalp res-array
                         #(1 2 3 4)))
      (read-sequence res-array (@
                                #(31 139 8 0 0 0 0 0 4
                                  3 99 100 98 102 1 0
                                  205 251 60 182 4 0 0 0)
                                 nil t))
      ;; binary data
      (fiasco:is (equalp res-array #(1 2 3 4))))))

(fiasco:deftest guess-encodings-from-content-type ()
  "Test encoding guesses based on the content-type."
  (let ((*default-encoding* :foo)
        (*default-text-encoding* :bar))
    (handler-bind ((warning 'muffle-warning))
      (fiasco:is (equalp (extract-charset-from-content-type "text/plain; charset=uTf-8")
                         :utf-8))
      (fiasco:is (equalp (extract-charset-from-content-type "text/plain; charset=latIn-2")
                         :bar))
      (fiasco:is (equalp (extract-charset-from-content-type "text/plain; charset=utf-8")
                         :utf-8))
      (fiasco:is (equalp (extract-charset-from-content-type "text/plain; charset=latin-2")
                         :bar))
      (fiasco:is (equalp (extract-charset-from-content-type "application/binary")
                         :foo))
      (fiasco:is (null (extract-charset-from-content-type "binary/whatever")))
      (fiasco:is (equalp (extract-charset-from-content-type nil)
                         :foo))
      (fiasco:is (equalp (extract-charset-from-content-type "text/plain")
                         :bar))
      (fiasco:is (equalp (extract-charset-from-content-type "application/json")
                         :utf-8)))))
