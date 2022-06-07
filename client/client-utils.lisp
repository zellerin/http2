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
                  (write-client-preface ,ssl-stream)
                  (write-settings-frame ,connection ,connection (get-settings ,connection))
                  (force-output ,ssl-stream)
                  ,@body)
             (close ,ssl-stream)))))))

(defun terminate-locally (connection &optional (code +no-error+))
  (write-goaway-frame connection connection 0 code #() 0)
  (setf (http2::get-finished connection) t))

;; 3.5.  HTTP/2 Connection Preface
(defvar +client-preface-start+
  #.(vector-from-hex-text "505249202a20485454502f322e300d0a0d0a534d0d0a0d0a")
  "The client connection preface starts with a sequence of 24 octets, which in hex notation is this. That is, the connection preface starts with the string
 \"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n\").")

(defun write-client-preface (stream)
  "In HTTP/2, each endpoint is required to send a connection preface as a
   final confirmation of the protocol in use and to establish the
   initial settings for the HTTP/2 connection.  The client and server
   each send a different connection preface.

   The client connection preface starts with a sequence of 24 octets.   This sequence MUST be followed by a SETTINGS frame (Section 6.5), which MAY be empty."
  (write-sequence +client-preface-start+ stream))
