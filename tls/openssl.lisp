(in-package #:http2/openssl)

(define-foreign-library openssl
  #-os-macosx (:unix "libssl.so")
  (t (:default "libssl.3")))

(export '(certificated-dispatcher make-http2-tls-context
          handle-ssl-errors* with-ssl-context encrypt-some* bio-should-retry
          certificate-file private-key-file))

(export '(bio-needs-read peer-closed-connection has-data-to-encrypt can-write-ssl
          can-read-bio bio-s-mem bio-new ssl-new ssl-set-accept-state ssl-set-bio ssl-free
          bio-write ssl-read% ssl-error-condition err-reason-error-string
          bio-read% ssl-is-init-finished ssl-accept))

(use-foreign-library openssl)

(defcfun "BIO_new" :pointer (bio-method :pointer))
(defcfun ("BIO_read" bio-read%) :int (bio-method :pointer) (data :pointer) (dlen :int))
(defcfun "BIO_s_mem" :pointer)
(defcfun "BIO_test_flags" :int (bio :pointer) (what :int))
(defcfun "BIO_write" :int (bio-method :pointer) (data :pointer) (dlen :int))

(defcfun "ERR_reason_error_string" :pointer (e :int))
(defcfun "ERR_get_error" :int)

(defcfun "SSL_accept" :int (ssl :pointer))
(defcfun "SSL_get_error" :int (ssl :pointer) (ret :int))
(defcfun "SSL_free" :int (ssl :pointer))
(defcfun "SSL_is_init_finished" :int (ssl :pointer))
(defcfun "SSL_new" :pointer (bio-method :pointer))
(defcfun "SSL_pending" :int (ssl :pointer))
(defcfun ("SSL_read" ssl-read%) :int (ssl :pointer) (buffer :pointer) (bufsize :int))
(defcfun "SSL_set_accept_state" :pointer (ssl :pointer))
(defcfun "SSL_set_bio" :void (ssl :pointer) (rbio :pointer) (wbio :pointer))
(defcfun "SSL_write" :int (ssl :pointer) (buffer :pointer) (bufsize :int))

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
           ;; h2 not found, but maybe server can handle
           (return ssl-tlsext-err-alert-fatal))) ; no agreement

(defcfun ("SSL_CTX_use_certificate_chain_file" ssl-ctx-use-certificate-chain-file)
  :int
  (ctx :pointer)
  (filename :string))

(defcfun ("SSL_CTX_use_PrivateKey_file" ssl-ctx-use-private-key-file)
  :int
  (ctx :pointer)
  (filename :string)
  (type :int))

(defclass certificated-dispatcher ()
  ((certificate-file :initarg  :certificate-file)
   (private-key-file :initarg  :private-key-file))
  (:documentation
   "Dispatcher with two slots, CERTIFICATE-FILE and PRIVATE-KEY-FILE, that are used
for TLS context creation."))

(defconstant +ssl-filetype-pem+ 1)
(defconstant +ssl-filetype-asn1+ 2)
(defconstant +ssl-filetype-default+ 3)

(defgeneric make-http2-tls-context (dispatcher)
  (:documentation "Make TLS context suitable for http2.

Practically, it means:
- ALPN callback that selects h2 if present,
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
      (ssl-ctx-set-alpn-select-cb  context (get-callback 'select-h2-callback))
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
  (:method :around ((dispatcher certificated-dispatcher))
    (with-slots (certificate-file private-key-file) dispatcher
      (let ((context (call-next-method)))
        (ssl-ctx-use-certificate-chain-file context certificate-file)
        (ssl-ctx-use-private-key-file context private-key-file +ssl-filetype-pem+)
        (unless (= 1 (ssl-ctx-check-private-key context))
          (error "server private/public key mismatch"))
        ;; TODO: we could check that hostname is compatible with the certificate
        ;; and warn if not.
        context))))

(defmacro with-ssl-context ((ctx dispatcher) &body body)
  "Run body with SSL context created by MAKE-SSL-CONTEXT in CTX."
  (check-type ctx symbol)
  `(let ((,ctx (make-http2-tls-context ,dispatcher)))
     (unwind-protect
          (progn ,@body)
       (ssl-ctx-free ,ctx))))
