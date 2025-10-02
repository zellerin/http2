(in-package #:http2/openssl)

(define-foreign-library openssl
  #-os-macosx (:unix "libssl.so")
  (t (:default "libssl.3")))

(defsection @openssl (:title "Openssl interface")
  "Wraps openssl calls."
  (@openssl-endpoint section)
  (@openssl-context section))

(export '(handle-ssl-errors* with-ssl-context encrypt-some* bio-should-retry))

(export '(bio-needs-read peer-closed-connection has-data-to-encrypt can-write-ssl
          can-read-bio
          ; bio-s-mem bio-new ssl-new
          ssl-set-accept-state
          bio-write ssl-read% ssl-error-condition err-reason-error-string
          bio-read% ssl-is-init-finished ssl-accept ssl-connect))

(defsection @openssl-endpoint (:title "TLS endpoint")
  (tls-endpoint-core struct)
  (init-tls-endpoint-core function)
  (make-tls-endpoint-core function)

  (close-openssl function))

(use-foreign-library openssl)

(defcfun "BIO_new" :pointer (bio-method :pointer))
(defcfun ("BIO_read" bio-read%) :int (bio-method :pointer) (data :pointer) (dlen :int))
(defcfun "BIO_s_mem" :pointer)
(defcfun "BIO_test_flags" :int (bio :pointer) (what :int))
(defcfun "BIO_write" :int (bio-method :pointer) (data :pointer) (dlen :int))

(defcfun "ERR_reason_error_string" :pointer (e :int))
(defcfun "ERR_get_error" :int)

(defcfun "SSL_accept" :int (ssl :pointer))
(defcfun "SSL_connect" :int (ssl :pointer))
(defcfun "SSL_get_error" :int (ssl :pointer) (ret :int))
(defcfun "SSL_free" :int (ssl :pointer))
(defcfun "SSL_is_init_finished" :int (ssl :pointer))
(defcfun "SSL_new" :pointer (bio-method :pointer))
(defcfun "SSL_pending" :int (ssl :pointer))
(defcfun ("SSL_read" ssl-read%) :int (ssl :pointer) (buffer :pointer) (bufsize :int))
(defcfun ("SSL_peek" ssl-peek%) :int (ssl :pointer) (buffer :pointer) (bufsize :int))
(defcfun "SSL_set_accept_state" :pointer (ssl :pointer))
(defcfun "SSL_set_bio" :void (ssl :pointer) (rbio :pointer) (wbio :pointer))
(defcfun "SSL_write" :int (ssl :pointer) (buffer :pointer) (bufsize :int))

(defstruct (tls-endpoint-core (:constructor make-tls-endpoint-core%))
  #+nil                         (:print-object
                                 (lambda (object out)
                                   (format out "#<client fd ~d, ~d octets to ~a>" (client-fd object)
                                           (client-octets-needed object) (client-io-on-read object))))
  "Data of one TLS endpoint. This includes:

- Opaque pointer to the openssl handle (SSL). See SSL-READ and ENCRYPT-SOME.
- Input and output BIO for exchanging data with OPENSSL (WBIO, RBIO)."
  (ssl (null-pointer) :type cffi:foreign-pointer :read-only nil) ; mostly RO, but invalidated afterwards
  (rbio (bio-new (bio-s-mem)) :type cffi:foreign-pointer :read-only t)
  (wbio (bio-new (bio-s-mem)) :type cffi:foreign-pointer :read-only t))

(defun init-tls-endpoint-core (client context)
  (let ((ssl (ssl-new context)))
    (ssl-set-bio ssl (tls-endpoint-core-rbio client) (tls-endpoint-core-wbio client))
    (setf (tls-endpoint-core-ssl client) ssl)))

(defun make-tls-endpoint-core (context)
  (let ((ep (make-tls-endpoint-core%)))
    (init-tls-endpoint-core ep context)
    ep))

(defmacro with-tls-endpoint-core ((name context) &body body)
  `(let ((,name (make-tls-endpoint-core ,context)))
     (declare (type tls-endpoint ,name))
     (unwind-protect
          (progn ,@body)
       (close-openssl ,name))))

(defun close-openssl (client)
  (unless (null-pointer-p (tls-endpoint-core-ssl client))
    (ssl-free (tls-endpoint-core-ssl client)))   ; BIOs are closed automatically
  (setf (tls-endpoint-core-ssl client) (null-pointer)))


(defsection @openssl-context (:title "TLS context")
  "TLS context is created with MAKE-HTTP2-TLS-CONTEXT, and its use should be
wrapped in WITH-SSL-CONTEXT."
  (with-ssl-context mgl-pax:macro)
  (make-tls-context generic-function)
  "The details of the context are modified by the context mixins.")

(defcfun "SSL_CTX_check_private_key" :int (ctx :pointer))
(defcfun "SSL_CTX_ctrl" :int (ctx :pointer) (cmd :int) (value :long) (args :pointer))
(defcfun "SSL_CTX_free" :int (ctx :pointer))
(defcfun "SSL_CTX_new" :pointer (method :pointer))
(defcfun "SSL_CTX_set_options" :int (ctx :pointer) (options :uint))
(defcfun "SSL_CTX_use_PrivateKey_file" :int (ctx :pointer) (path :string) (type :int))
(defcfun "SSL_CTX_use_certificate_file" :int (ctx :pointer) (path :string) (type :int))
(defcfun ("SSL_CTX_set_alpn_select_cb" ssl-ctx-set-alpn-select-cb) :void
  (ctx :pointer)
  (alpn-select-cb :pointer))
(defcfun "SSL_CTX_set_cipher_list" :int (ctx :pointer) (str :string))
(defcfun "SSL_CTX_set_ciphersuites" :int (ctx :pointer) (str :string))

(defcfun "TLS_method" :pointer)



(defcfun ("SSL_CTX_use_certificate_chain_file" ssl-ctx-use-certificate-chain-file)
  :int
  (ctx :pointer)
  (filename :string))

(defcfun ("SSL_CTX_use_PrivateKey_file" ssl-ctx-use-private-key-file)
  :int
  (ctx :pointer)
  (filename :string)
  (type :int))


#+unused
(defcfun ("SSL_select_next_proto" ssl-select-next-proto)
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
           ;; h2 not found, alert
           (return ssl-tlsext-err-alert-fatal))) ; no agreement

(defclass h2-server-context-mixin ()
  ()
  (:documentation "This mixin ensures that the server will provide H2 alpn during TLS negotiation."))

(defconstant +ssl-filetype-pem+ 1)
(defconstant +ssl-filetype-asn1+ 2)
(defconstant +ssl-filetype-default+ 3)

(defgeneric make-tls-context (dispatcher)
  (:documentation "Make TLS context suitable for http2, depending on DISPATCHER.

The specialations include
:
- ALPN callback that selects h2 (h2-server-context-mixin)
- Do not request client certificates
- Do not allow ssl compression and renegotiation.
We should also limit allowed ciphers, but we do not.")
  (:method (dispatcher)
    (let ((context (ssl-ctx-new (tls-method))))
      (when (null-pointer-p context)
        (error "Could not create context"))
                                        ; FIXME: cl+ssl has (apply #'logior (append disabled-protocols options)
      (ssl-ctx-set-options context ssl-op-all)
      (ssl-ctx-ctrl context ssl-ctrl-set-min-proto-version tls-1.2-version (null-pointer))
      #+nil    (ssl-ctx-set-session-cache-mode context session-cache-mode)
      #+nil    (ssl-ctx-set-verify-location context verify-location)
      #+nil    (ssl-ctx-set-verify-depth context verify-depth)
      #+nil    (ssl-ctx-set-verify context verify-mode (if verify-callback
                                                           (cffi:get-callback verify-callback)
                                                           (cffi:null-pointer)))

      #+nil    (when (and cipher-list
                          (zerop (ssl-ctx-set-cipher-list context cipher-list)))
                 (error "Cannot set cipher list"))
      context))
  (:method ((dispatcher h2-server-context-mixin))
    "For servers"
    (let ((context (call-next-method)))
      (ssl-ctx-set-alpn-select-cb  context (get-callback 'select-h2-callback))
      context))

  (:method ((dispatcher certificated-context-mixin))
    (with-slots (certificate-file private-key-file) dispatcher
      (let ((context (call-next-method)))
        (ssl-ctx-use-certificate-chain-file context certificate-file)
        (ssl-ctx-use-private-key-file context private-key-file +ssl-filetype-pem+)
        (unless (= 1 (ssl-ctx-check-private-key context))
          (error "server private/public key mismatch"))
        ;; TODO: we could check that hostname is compatible with the certificate
        ;; and warn if not.
        context))))

(defun make-http2-tls-context (dispatcher)
  "Do not use that. Changed name, as not http2 specific (use proper mixins for it)."
  (make-tls-context dispatcher))

(declaim (sb-ext:deprecated :early ("http2" "2.0.3")
                            (function make-http2-tls-context :replacement make-tls-context)))

(defmacro with-ssl-context ((ctx dispatcher) &body body)
  "Run body with SSL context created by MAKE-TLS-CONTEXT in CTX."
  (check-type ctx symbol)
  `(let ((,ctx (make-tls-context ,dispatcher)))
     (unwind-protect
          (progn ,@body)
       (ssl-ctx-free ,ctx))))
