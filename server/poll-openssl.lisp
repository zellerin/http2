(in-package #:http2/openssl)

(defsection @SSL2 (:title "SSL handling")
  (communication-error condition))

(defun process-ssl-errors ()
  "Get SSL error and either close connection immediately (for some known and
expected errors) or let user handle it."
  (loop for err = (err-get-error)
        until (zerop err)
        if (or (position err #(#xa000412 #xA00042E
                               #xA000418
                               #xA00010B #xa000139))) ;bad certificate, protocol version
          do (invoke-restart 'http2/core:close-connection
                             'ssl-error-condition :code err)
        do
           (cerror "Ignore" 'ssl-error-condition :code err)))


(define-condition simple-communication-error (simple-condition communication-error)
  ())

(define-condition peer-closed-tls (communication-error)
  ())


(defun handle-ssl-errors* (client ret)
  "Check RET value of a openssl call. Either raise a condition, or return a state to add to the client, if any, or nil"
  (let* ((ssl (tls-endpoint-core-ssl client))
         (wbio (tls-endpoint-core-rbio client))
         (err-code (ssl-get-error ssl ret)))
    (cond
      ;; after ssl read
      ((= err-code ssl-error-want-write)
       (when (zerop (bio-test-flags wbio bio-flags-should-retry))
         (error 'simple-communication-error :format-control "Retry flag should be set."
                                            :medium client))
       (warn "This path wath not tested.")
       nil)
      ((= err-code ssl-error-want-read)
       ;; This is relevant for accept call and handled in loop
       ;; may be needed for pull phase
       ;; is this needed?
       (when (zerop (bio-test-flags wbio bio-flags-should-retry))
         (error 'simple-communication-error :format-control "Retry flag should be set."
                                            :medium client))
       'neg-bio-needs-read)
      ((= err-code ssl-error-none) nil)
      ((= err-code ssl-error-zero-return)
       ;; Peer closed TLS connection
       (error 'peer-closed-tls :medium client))
      ((= err-code ssl-error-ssl)
       (process-ssl-errors)
       nil)
      ((= err-code ssl-error-syscall)
       (let ((errno (http2/tcpip:errno)))
         (unless (zerop errno)
           ;; IIUC end of communication gives errno 0
           (warn "SSL syscall error ~d (~a)" errno (http2/server/poll::strerror errno))))
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
                  (error 'simple-communication-error
                         :format-control "Peer send close notify alert. One possible cause - it does not like our certificate"))
                 ((= status ssl-error-syscall)
                  (let ((errno (http2/tcpip:errno)))
                    (unless (zerop errno)
                      ;; IIUC end of communication gives errno 0
                      (warn "SSL syscall error ~d (~a)" errno (http2/server/poll::strerror errno))))                  )
                 (t (error "SSL write failed, status ~d" status))))))))

(defun bio-should-retry (wbio)
  (bio-test-flags wbio bio-flags-should-retry))
