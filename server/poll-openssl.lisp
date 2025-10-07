(in-package #:http2/openssl)

(defsection @SSL2 (:title "SSL handling")
  (communication-error condition)
  (ssl-blocked condition)
  (ssl-wants-read condition)
  (ssl-wants-write condition))

(define-condition ssl-error-condition (error)
  ((code :accessor get-code :initarg :code))
  (:documentation "The socket on the other side is closed.

Check the code in openssl/sslerr.h "))

(defmethod print-object ((object ssl-error-condition) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~x: ~a" (get-code object)
            (foreign-string-to-lisp (err-reason-error-string (get-code object))))))

(defun process-ssl-errors ()
  "Get SSL error and either close connection immediately (for some known and
expected errors) or let user handle it."
  (loop for err = (err-get-error)
        until (zerop err)
        if (or (position err #(#xa000412 #xA00042E
                               #xA000418
                               #xA00010B #xa000139))) ;bad certificate, protocol version
          do (error 'ssl-error-condition :code err)
        do
           (cerror "Ignore" 'ssl-error-condition :code err)))


(define-condition simple-communication-error (simple-condition communication-error)
  ())

(define-condition peer-sent-alert (communication-error)
  ()
  (:documentation  "Peer send close notify alert. One possible cause - it does not like our certificate."
   ))

(define-condition ssl-blocked (communication-error)
  ())

(define-condition ssl-wants-read (ssl-blocked)
  ()
  (:documentation  ""))

(define-condition ssl-wants-write (ssl-blocked)
  ()
  (:documentation  ""))


(defun handle-ssl-errors* (client ret)
  "Check RET value of a openssl call and raise relevant error.."
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
      ((= err-code ssl-error-none) nil)
      ((= err-code ssl-error-zero-return) (error 'peer-sent-alert :medium client))
      ((= err-code ssl-error-ssl) (process-ssl-errors) nil)
      ((= err-code ssl-error-syscall)
       (let ((errno (http2/tcpip:errno)))
         (unless (zerop errno)
           ;; IIUC end of communication gives errno 0
           (error 'http2/tcpip:syscall-error :errno errno :medium client)))
;       (invoke-restart 'http2/core:close-connection)
       )
      (t (warn "SSL error ~d" err-code)
         (invoke-restart 'http2/core:close-connection)))))

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
