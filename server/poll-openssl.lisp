(in-package #:http2/openssl)

(defsection @SSL (:title "SSL handling")
            (communication-error condition)
  (@ssl-errors section))

(defsection @SSL-errors (:title "Signalled errors")
  "Error conditions from openssl api calls are represented by appropriate error
conditions. They are descended from the COMMUNICATION-ERROR.

See manual page for SSL_get_error for the overview."
  (communication-error condition)
  (ssl-blocked condition)
  (ssl-wants-read condition)
  (ssl-wants-write condition))

(define-condition ssl-error-condition (communication-error)
  ((codes :accessor get-codes :initarg :codes))
  (:documentation "An error condition on SSL side that is not handled separately.

The list of error codes is in openssl/sslerr.h.

A non-recoverable, fatal error in the SSL library occurred, usually a protocol
 error.  The OpenSSL error queue contains more information on the error. If this
 error occurs then no further I/O operations should be performed on the
 connection and SSL_shutdown() must not be called."))

(defmethod print-object ((object ssl-error-condition) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (dolist (code (get-codes object))
      (format stream "~x: ~a" code
              (foreign-string-to-lisp (err-reason-error-string code))))
    (format stream " on ~a" (get-medium object))))

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
indicate that the underlying transport has been closed."))

(define-condition ssl-blocked (communication-error)
  ()
  (:documentation "The operation did not complete and can be retried later."))

(define-condition ssl-wants-read (ssl-blocked)
  ()
  (:documentation
   "The last operation was a read operation from a nonblocking BIO. Not enough data
was available at this time to complete the operation.  If at a later time the
underlying BIO has data available for reading the same function can be called
again."))

(define-condition ssl-wants-write (ssl-blocked)
  ()
  (:documentation  ""))

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

(defun handle-ssl-errors* (client ret)
  "Check RET value of a openssl call and raise relevant error, if any."
  (let* ((ssl (tls-endpoint-core-ssl client))
         (wbio (tls-endpoint-core-rbio client))
         (err-code (ssl-get-error ssl ret)))
    (cond
      ;; after ssl read
      ((= err-code ssl-error-want-write)
       (when (zerop (bio-test-flags wbio bio-flags-should-retry))
         (error 'simple-communication-error :format-control "Retry flag should be set."
                                            :medium client))
       (error 'ssl-wants-write :medium client))
      ((= err-code ssl-error-want-read)
       ;; This is relevant for accept call and handled in loop
       ;; may be needed for pull phase
       ;; is this needed?
       (when (zerop (bio-test-flags wbio bio-flags-should-retry))
         (error 'simple-communication-error :format-control "Retry flag should be set."
                                            :medium client))
       (error 'ssl-wants-read))
      ((= err-code ssl-error-none) nil) ; this should happen iff ret > 0
      ((= err-code ssl-error-zero-return) (error 'peer-sent-close-notify :medium client))
      ((= err-code ssl-error-ssl) (error 'ssl-error-condition :medium client :codes (get-ssl-errors)))
      ((= err-code ssl-error-syscall)
       (let ((errno (http2/tcpip:errno)))
         (if (zerop errno)
             (error 'unexpected-eof :medium client)
             (error 'ssl-syscall-error :codes (get-ssl-errors) :errno errno :medium client))))
      (t (error 'other-ssl-error :code err-code :medium client)))))

(defun encrypt-some* (client vector from to)
  "Encrypt octets in VECTOR between FROM and TO. Return number of octets
processed, or raise appropriate error."
  (with-pointer-to-vector-data (buffer vector)
    (let* ((ssl (tls-endpoint-core-ssl client))
           (res (ssl-write ssl (inc-pointer buffer from) (- to from))))
      (cond
        ((plusp res) res)
        (t (handle-ssl-errors* client res)
           0)))))

(defun bio-should-retry (wbio)
  (bio-test-flags wbio bio-flags-should-retry))
