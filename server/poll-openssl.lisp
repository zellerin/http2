(in-package #:http2/openssl)

(defsection @SSL (:title "SSL handling")
  "Wrapper library over openssl functions.

TLS-ENDPOINT-CORE wraps the SSL parameter used in openssl functions. The functions that use it are listed in @SSL-OPS.

"
#+nil  (bio-should-retry function)
  (@openssl-endpoint section)
  (@openssl-context section)
  (@ssl-ops section)
  (@ssl-errors section))

(defsection @SSL-errors (:title "Signalled errors")
  "Error conditions from openssl api calls are represented by appropriate error
conditions. They are descended from the COMMUNICATION-ERROR.

See manual page for SSL_get_error for the overview."
  (handle-ssl-errors function)
  (communication-error condition)
  (simple-communication-error condition)
  (peer-sent-close-notify condition)
  (ssl-error-condition condition)
  (ssl-syscall-error condition)
  (other-ssl-error condition))

(define-condition ssl-error-condition (communication-error)
  ((codes :accessor get-codes :initarg :codes))
  (:report (lambda (condition out)
             (format out "Peer sent an alert: ~{~a~%~}"
                     (mapcar (lambda (code)
                               (foreign-string-to-lisp (err-reason-error-string code)))
                             (get-codes condition)))))
  (:documentation "An error condition on SSL side that is not handled separately.

The list of error codes is in openssl/sslerr.h.

A non-recoverable, fatal error in the SSL library occurred, usually a protocol
 error.  The OpenSSL error queue contains more information on the error. If this
 error occurs then no further I/O operations should be performed on the
 connection and SSL_shutdown() must not be called."))

(defmethod print-object ((object ssl-error-condition) stream)
  (if *print-escape*
      (print-unreadable-object (object stream :type t :identity nil)
        (dolist (code (get-codes object))
          (format stream "~x: ~a" code
                  (foreign-string-to-lisp (err-reason-error-string code))))
        (format stream " on ~a" (get-medium object)))
      (call-next-method)))

(defun get-ssl-errors ()
  "Get SSL error and either close connection immediately (for some known and
expected errors) or let user handle it."
  (loop for err = (err-get-error)
        until (zerop err)
        collect err))

(define-condition ssl-syscall-error (ssl-error-condition http2/tcpip:syscall-error)
  ()
  (:documentation "Some non-recoverable, fatal I/O error occurred. The OpenSSL error
 queue may contain more information on the error. For socket I/O on Unix
 systems, consult errno for details. If this error occurs then no further I/O
 operations should be performed on the connection and SSL_shutdown() must not be
 called.

 This value can also be returned for other errors, check the error queue for
 details."))

(define-condition simple-communication-error (simple-condition communication-error)
  ())

(define-condition peer-sent-alert (communication-error)
  ()
  (:documentation  "The TLS/SSL peer has closed the connection for writing by sending the
close_notify alert.  No more data can be read.  This does not necessarily
indicate that the underlying transport has been closed."))

(define-condition peer-sent-close-notify (peer-sent-alert)
  ()
  (:documentation  "The TLS/SSL peer has closed the connection for writing by sending the
close_notify alert.  No more data can be read.  This does not necessarily
indicate that the underlying transport has been closed.")
  ;; To test: run a curl request
  (:report "Peer closed TLS connection."))

(define-condition retry-flag-not-set (communication-error)
  ()
  (:documentation "Openss "))

(define-condition unexpected-eof (communication-error)
  ()
  (:documentation
   "On an unexpected EOF, versions before OpenSSL 3.0 returned SSL_ERROR_SYSCALL,
nothing was added to the error stack, and errno was 0."))

(define-condition other-ssl-error (communication-error)
  ((code :accessor get-code :initarg :code))
  (:documentation "ssl-get-error return code that we do not handle (yet)"))

(defun bio-should-retry (wbio client)
  (when (zerop (bio-test-flags wbio bio-flags-should-retry))
    (error 'simple-communication-error :format-control "Retry flag should be set."
                                       :medium client)))

(defun handle-ssl-errors* (client ret on-read-fail-idx on-write-fail-idx)
  "Raise appropriate error after a failed openssl call.

If ret>0 (no fail), returns nil. If write or read is needed by SSL, return nil
as well and clear appropriate flag.

Otherwise raise one of SIMPLE-COMMUNICATION-ERROR, PEER-SENT-CLOSE-NOTIFY,
SSL-ERROR-CONDITION, SSL-SYSCALL-ERROR, or OTHER-SSL-ERROR.

"
  ;; after SSL_connect(), SSL_accept(),SSL_do_handshake(), SSL_read_ex(),
  ;; SSL_read(), SSL_peek_ex(),SSL_peek(), SSL_shutdown(), SSL_write_ex() or
  ;; SSL_write()

  (let* ((ssl (tls-endpoint-core-ssl client))
         (wbio (tls-endpoint-core-rbio client))
         (err-code (ssl-get-error ssl ret)))
    (cond
      ;; after ssl read
      ((= err-code ssl-error-want-write)
       (bio-should-retry wbio client)
       (set-state* client on-write-fail-idx 0))
      ((= err-code ssl-error-want-read)
       ;; This is relevant for accept call and handled in loop
       ;; may be needed for pull phase
       ;; is this needed?
       (bio-should-retry wbio client)
       (set-state* client on-read-fail-idx 0))
      ((= err-code ssl-error-none) nil) ; this should happen iff ret > 0
      ((= err-code ssl-error-zero-return) (error 'peer-sent-close-notify :medium client))
      ((= err-code ssl-error-ssl) (error 'ssl-error-condition :medium client :codes (get-ssl-errors)))
      ((= err-code ssl-error-syscall)
       (let ((errno (http2/tcpip:errno)))
         (if (zerop errno)
             (error 'unexpected-eof :medium client)
             (error 'ssl-syscall-error :codes (get-ssl-errors) :errno errno :medium client))))
      (t (error 'other-ssl-error :code err-code :medium client)))))

(defun handle-ssl-errors (client ret)
  "Check real error after a call to SSL_connect, SSL_accept,
SSL_do_handshake, SSL_read_ex, SSL_read, SSL_peek_ex, SSL_peek, SSL_shutdown,
SSL_write_ex or SSL_write.

If the operation was successfully completed, do nothing.

If it is a harmless one (want read or want write), try to process the data.

Raise error otherwise."
  (handle-ssl-errors* client ret (state-idx 'can-read-ssl) nil)) ; FIXME: write status


(defsection @ssl-ops ()
  "Use ENCRYPT-SOME* and SSL-READ"
  (encrypt-some* function)
  (read-encrypted-from-openssl* function)
  (write-octets-to-decrypt* function)
  (ssl-read function)
  (maybe-init-ssl function)
  (ssl-peek function))

; TODO: rename to encrypt-vector
(defun encrypt-some* (client vector from to)
  "Encrypt octets in VECTOR between FROM and TO. Return number of octets
processed, or raise appropriate error. You can try to read the encrypted octets
later by READ-ENCRYPTED-FROM-OPENSSL*, and it sets CAN-READ-BIO."
  ;; VECTOR -> SSL
  (declare (type (integer 0 #.array-dimension-limit) from to))
  (assert (if-state client 'can-write-ssl))
  (with-pointer-to-vector-data (buffer vector)
    (let* ((ssl (tls-endpoint-core-ssl client))
           (res (ssl-write ssl (inc-pointer buffer from) (- to from))))
      (cond
        ((plusp res)
         (add-state client 'can-read-bio)
         res)
        ;; no-star handle-ssl-errors masks SSL-WANTS-READ
        (t (handle-ssl-errors* client res (state-idx 'can-write-ssl) nil)
           0)))))

(defun read-encrypted-from-openssl* (client vec size)
  "Read decrypted octets from WBIO. Possibly cleans CAN-READ-BIO.

Return number of octets read"
  ;; WBIO -> VEC
  (declare ((simple-array (unsigned-byte 8)) vec)
           (fixnum size))
  (assert (if-state client 'can-read-bio))
  (with-pointer-to-vector-data (buffer vec)
    (let* ((wbio (tls-endpoint-core-wbio client))
           (res (bio-read% wbio buffer size)))
      (cond ((plusp res)
             (add-state client 'can-read-ssl)
             (add-state client 'can-write-ssl)
             res)
            (t
             (bio-should-retry wbio client)
             (remove-state client 'can-read-bio)
             0)))))

(defun write-octets-to-decrypt* (client vector from to)
  "Send octets in VECTOR for decryption. Read result with SSL-READ later.

Return number of written bytes. This should be positive.

When SSL receives data through the BIO channel, it possibly can be read from and
written to again (or at least it could be tried).

This assumes writing BIO never fails due to size."
  (with-pointer-to-vector-data (buffer vector)
    (let ((written (bio-write (tls-endpoint-core-rbio client)
                              (inc-pointer buffer from)
                              (- to from))))
      (unless (plusp written) (error "Bio-write failed"))
      (add-state client 'can-read-ssl)
      (add-state client 'can-read-bio) ; maybe ssl needs to do something?
      (add-state client 'can-write-ssl)
      written)))

(defun maybe-init-ssl (client)
  "If SSL is not initialized yet, initialize it."
  (cond
    ((zerop (ssl-is-init-finished (tls-endpoint-core-ssl client)))
     (handle-ssl-errors client (ssl-accept (tls-endpoint-core-ssl client))))
    (t (remove-state client 'ssl-init-needed)
       (add-state client 'can-read-bio)
       (add-state client 'can-read-ssl)
       (add-state client 'can-write-ssl))))

;;;; Read decrypted data
(defun ssl-read (client vec size)
   "Move up to SIZE octets from the decrypted SSL ③ to the VEC.

Return 0 when no data are available. Possibly remove CAN-READ-SSL flag."
    (assert (if-state client 'can-read-ssl))
  (let ((res
          (with-pointer-to-vector-data (buffer vec)
            (ssl-read% (tls-endpoint-core-ssl client) buffer size))))
    (handle-ssl-errors client res)
    (unless (= res size) (remove-state client 'can-read-ssl))
    (max 0 res)))

(defun ssl-peek (client max-size)
   "Copy up to SIZE octets from the decrypted SSL ③ to the VEC.

This is intended for introspection and debugging, e.g., in DESCRIBE-OBJECT.

Return 0 when no data are available."
  (unless (null-pointer-p (tls-endpoint-core-ssl client))
    (let* ((vec (make-octet-buffer max-size))
           (res
             (with-pointer-to-vector-data (buffer vec)
               (ssl-peek% (tls-endpoint-core-ssl client) buffer max-size))))
      (handle-ssl-errors client res)
      (values (subseq vec 0 (max 0 res)) res))))
