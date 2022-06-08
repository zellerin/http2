(in-package http2)

(defmacro with-http-connection ((connection target &key (sni target) (port 443)
                                          (connection-class ''http2-connection)
                                          (verify nil))
                                &body body)
  "Run BODY with established HTTP2 connection over TLS to PORT on
TARGET, using SNI."
  (alexandria:with-gensyms (socket stream ssl-stream)
    `(usocket:with-client-socket (,socket ,stream ,target ,port )
       (let* ((,ssl-stream (cl+ssl:make-ssl-client-stream
                           ,stream
                           :verify ,verify
                           :hostname ,sni
                           :alpn-protocols '("h2")))
              (,connection (make-instance ,connection-class :network-stream ,ssl-stream)))
         (unless (equal (cl+ssl:get-selected-alpn-protocol ,ssl-stream) "h2")
           (error "HTTP/2 not supported by ~a" ,sni))
         (flet ((wait-for-responses ()
                  (force-output (get-network-stream ,connection))
                  (with-simple-restart (use-read-so-far "Use data read so far")
                    (handler-case
                        (loop
                          do
                             (read-frame ,connection)
                          until (get-finished ,connection))
                      (end-of-file () nil)))))
           (unwind-protect
                (progn
                  ,@body)
             (close ,ssl-stream)))))))

(defun terminate-locally (connection &optional (code +no-error+))
  (write-goaway-frame connection connection 0 code #())
  (setf (http2::get-finished connection) t))
