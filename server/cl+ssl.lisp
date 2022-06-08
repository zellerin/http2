(in-package cl+ssl)

(define-ssl-function ("SSL_CTX_set_alpn_select_cb" ssl-ctx-set-alpn-select-cb)
    :void
  (ctx ssl-ctx)
  (alpn-select-cb :pointer)
#+nil  (args :pointer))

(cffi:defcallback select-h2-callback
    :int
    ((ssl :pointer)
     (out (:pointer (:pointer :char)))
     (outlen (:pointer :char))
     (in (:pointer :char))
     (inlen :int)
     (args :pointer))
  (declare (ignore args ssl))
  (loop for idx from 0 to (- inlen 3)
        when (and (= (cffi:mem-ref in :char idx) 2)
                  (= (cffi:mem-ref in :char (+ 1 idx)) (char-code #\h))
                  (= (cffi:mem-ref in :char (+ 2 idx)) (char-code #\2)))
          do
             (setf
              (cffi:mem-ref outlen :char) 2
              (cffi:mem-ref out :pointer) (cffi:inc-pointer in (1+ idx)))
             (return 0)
        finally
           (print "H2 not found!")
           (return 1)))
