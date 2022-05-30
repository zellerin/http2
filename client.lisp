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
         (unwind-protect
              (progn
                (write-client-preface ,ssl-stream)
                (write-settings-frame ,connection ,connection (get-settings ,connection))
                (force-output ,ssl-stream)
                (read-frame ,connection ,ssl-stream) ; should be settings frame
                (unless (get-peer-settings ,connection)
                  (error "We expected settings frame"))
                (write-ack-setting-frame ,ssl-stream)
                ,@body)
           (close ,ssl-stream))))))

(defclass client-connection (http2-connection)
  ((finished :accessor get-finished :initarg :finished
             :initform nil))
  (:default-initargs :stream-class 'client-stream))

(defclass client-stream (http2-stream log-headers-mixin)
  ((content-type :accessor get-content-type :initarg :content-type)
   (data         :accessor get-data         :initarg :data)))

(defclass log-headers-mixin ()
  ())

(defmethod add-header :after ((stream log-headers-mixin) name value)
  (format t "~&header: ~a = ~a~%" name value))

(defun retrieve-url (url &key (method "GET"))
  "Retrieve URL through http/2 over TLS."
  (let ((parsed-url (puri:parse-uri url)))
    (with-http-connection (connection (puri:uri-host parsed-url)
                           :port (or (puri:uri-port parsed-url) 443)
                           :connection-class 'client-connection)
      (let ((http-stream (create-new-local-stream connection)))
        (write-headers-frame connection
                             http-stream
                             (request-headers method
                                              (or (puri:uri-path parsed-url) "/")
                                              (puri:uri-host parsed-url))
                             :end-headers nil :end-stream t)
        ;; just to test continuation frame
        (write-continuation-frame connection
                                  http-stream
                                  (list (encode-header "user-agent" "CL/custom"))
                                  :end-headers t)
        ;; and test ping
        (write-ping-frame connection connection nil (vector 0 1 2 3 4 5 6 7))
        ;; and test go-away
        (write-goaway-frame connection connection 0 +no-error+ #() nil))
      (force-output (get-network-stream connection))
      (loop
        do
           (read-frame connection)
        #+nil        until (get-finished connection)))))

(defmethod apply-data-frame ((connection client-connection) stream payload)
  (map nil (lambda (c) (princ (code-char c))) payload))

(defmethod process-end-stream :after ((connection client-connection) stream)
  (setf (get-finished connection) t))

(defun init-tls-connection (target &key (sni target)  (port 443))
"The client sends the client connection preface (...) as the first
application data octets of a TLS connection.

   The server connection preface consists of a potentially empty
   SETTINGS frame (Section 6.5) that MUST be the first frame the server
   sends in the HTTP/2 connection.

   The SETTINGS frames received from a peer as part of the connection
   preface MUST be acknowledged (see Section 6.5.3) after sending the
   connection preface.
   To avoid unnecessary latency, clients are permitted to send
   additional frames to the server immediately after sending the client
   connection preface, without waiting to receive the server connection
   preface.  It is important to note, however, that the server
   connection preface SETTINGS frame might include parameters that
   necessarily alter how a client is expected to communicate with the
   server.  Upon receiving the SETTINGS frame, the client is expected to
   honor any parameters established.  In some configurations, it is
   possible for the server to transmit SETTINGS before the client sends
   additional frames, providing an opportunity to avoid this issue.

   Clients and servers MUST treat an invalid connection preface as a
   connection error (Section 5.4.1) of type PROTOCOL_ERROR.  A GOAWAY
   frame (Section 6.8) MAY be omitted in this case, since an invalid
   preface indicates that the peer is not using HTTP/2."
  (usocket:with-client-socket (socket stream target port)
    (let ((ssl-stream (cl+ssl:make-ssl-client-stream
                       stream
                       :verify nil
                       :hostname sni
                       :alpn-protocols '("h2")))
          (connection (make-instance 'http2-connection)))
      (write-client-preface ssl-stream)
      (write-settings-frame connection ssl-stream (get-settings connection))
      (force-output ssl-stream)
      (read-frame connection ssl-stream) ; should be settings frame
      (unless (get-peer-settings connection)
        (error "We expected settings frame"))
      (write-ack-setting-frame ssl-stream)
      (write-headers-frame ssl-stream
                           (create-new-local-stream connection)
                           (request-headers "GET"  "/" sni)
                           :end-headers t :end-stream t)
      (force-output ssl-stream)
      (read-frame connection ssl-stream)
      (multiple-value-bind (payload http-stream type flags)
          (read-frame connection ssl-stream)
        (values connection payload http-stream type flags))
      (read-frame connection ssl-stream)
      (multiple-value-bind (payload http-stream type flags)
          (read-frame connection ssl-stream)
        (close ssl-stream)
        (close stream)
        (values connection payload http-stream type flags)))))
