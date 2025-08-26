(in-package #:http2/openssl)

(defun process-ssl-errors ()
  (loop for err = (err-get-error)
        until (zerop err)
        unless (position err #(#xa000412 )) ;bad certificate
          do
             (cerror "Ignore" 'ssl-error-condition :code err)))

(defun handle-ssl-errors* (ssl wbio ret)
  "Check RET value of a openssl call. Either raise a condition, or return a state to add to the client, if any, or nil"
  (let ((err-code (ssl-get-error ssl ret)))
    (cond
      ;; after ssl read
      ((= err-code ssl-error-want-write)
       (when (zerop (bio-test-flags wbio bio-flags-should-retry))
         (error "Retry flag should be set."))
       (warn "This path wath not tested.")
       nil)
      ((= err-code ssl-error-want-read)
       ;; This is relevant for accept call and handled in loop
       ;; may be needed for pull phase
       ;; is this needed?
       (when (zerop (bio-test-flags wbio bio-flags-should-retry))
         (error "Retry flag should be set."))
       'bio-needs-read)
      ((= err-code ssl-error-none) nil)
      ((= err-code ssl-error-zero-return)
       ;; Peer closed TLS connection
       'peer-closed-connection)
      ((= err-code ssl-error-ssl)
       (process-ssl-errors)
       nil)
      ((= err-code ssl-error-syscall)
       (let ((errno (http2/server/poll::errno)))
         (warn "SSL syscall error ~d (~a)" errno (http2/server/poll::strerror errno)))
       (invoke-restart 'http2/core:close-connection))
      (t (warn "SSL error ~d" err-code)
         (invoke-restart 'http2/core:close-connection)))))

(defun encrypt-some* (ssl pointer size)
  (let* ((res (ssl-write ssl pointer size)))
    (cond
      ((plusp res)
       (values res 'can-read-bio nil))
      (t (let ((status (ssl-get-error ssl res)))
           (cond ((member status
                          (list ssl-error-none ssl-error-want-write ssl-error-want-read))
                  (values 0  'has-data-to-encrypt 'can-write-ssl))
                 ((= status ssl-error-zero-return)
                  (warn "Peer send close notify alert. One possible cause - it does not like our certificate")
                  (signal 'http2/server/poll::done))
                 (t (error "SSL write failed, status ~d" status))))))))

(defun bio-should-retry (wbio)
  (bio-test-flags wbio bio-flags-should-retry))
