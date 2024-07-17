;;;; Copyright 2022 by Tomáš Zellerin

(in-package :cl+ssl)

(defconstant +SSL-TLSEXT-ERR-ALERT-FATAL+ 2)

#+add-when-defsection  (cl+ssl::ssl-error condition)

(define-ssl-function ("SSL_CTX_set_alpn_select_cb" ssl-ctx-set-alpn-select-cb)
    :void
  (ctx ssl-ctx)
  (alpn-select-cb :pointer)
#+nil  (args :pointer))

(define-ssl-function ("SSL_select_next_proto" ssl-select-next-proto)
  :int
  (out (:pointer (:pointer :char)))
  (outlen (:pointer :char))
  (server (:pointer :char))
  (serverlen :int)
  (client (:pointer :char))
  (clientlen :int))

(cffi:defcallback select-h2-callback
    :int
    ((ssl :pointer)
     (out (:pointer (:pointer :char)))
     (outlen (:pointer :char))
     (in (:pointer :char))
     (inlen :int)
     (args :pointer))
  ;; this is basically reimplemented SSL_select_next_proto, but easier than to
  ;; use that one in ffi world.
  "Set ALPN to h2 if it was offered, otherwise to the first offered."
  (declare (ignore args ssl))
  #+nil
  (cffi:with-foreign-string ((server serverlen) (make-alpn-proto-string '("h2")))
    (ssl-select-next-proto out outlen server (print (1- serverlen)) in inlen)
    0)

  (loop for idx = 0 then (+ (cffi:mem-ref in :char idx) idx)
        while (< idx inlen)
        when (and (= (cffi:mem-ref in :char idx) 2)
                  (= (cffi:mem-ref in :char (+ 1 idx)) (char-code #\h))
                  (= (cffi:mem-ref in :char (+ 2 idx)) (char-code #\2)))
          do
             (setf
              (cffi:mem-ref outlen :char) 2
              (cffi:mem-ref out :pointer) (cffi:inc-pointer in (1+ idx)))
             (return 0)
        finally
           ;; h2 not found, but maybe server can handle
           ;; set the proto to first offered
           (setf
            (cffi:mem-ref outlen :char) (cffi:mem-ref in :char 0)
            (cffi:mem-ref out :pointer) (cffi:inc-pointer in 1))
           (return 0)))
