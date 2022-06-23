;;;; Copyright 2022 by Tomáš Zellerin

;;;; define VANILLA-CLIENT-CONNECTION with relatively sane client side
;;;; behaviour. Define WITH-HTTP-CONNECTION macro that allows to talk to other
;;;; ports.
(in-package :http2)

(defmacro with-http-connection ((connection target &key (sni target) (port 443)
                                          (connection-class ''vanilla-client-connection)
                                          (verify nil))
                                &body body)
  "Run BODY with established HTTP2 connection over TLS to PORT on
TARGET, using SNI."
  (alexandria:with-gensyms (socket stream ssl-stream)
    `(usocket:with-client-socket (,socket ,stream ,target ,port
                                          :element-type '(unsigned-byte 8))
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
                          until (and (get-finished ,connection)
                                     (null (listen (get-network-stream ,connection)))))
                      (end-of-file () nil)))))
           (unwind-protect
                (progn
                  ,@body)
             (close ,ssl-stream)))))))

(defun terminate-locally (connection &optional (code +no-error+))
  (write-goaway-frame connection connection 0 code #())
  (force-output (get-network-stream connection))
  (setf (http2::get-finished connection) t))

(defclass vanilla-client-connection (client-http2-connection http2::history-printing-object
                                    http2::timeshift-pinging-connection)
  ((finished :accessor http2::get-finished :initarg :finished
             :initform nil))
  (:default-initargs :stream-class 'vanilla-client-stream))

(defclass vanilla-client-stream (client-stream ;; basic semantics and pseudoheaders
                                 ;; recieved data are stored in slot BODY
                                 http2::body-collecting-mixin
                                 ;; headers (not pseudoheaders) are collected in
                                 ;; slot HEADERS
                                 http2::header-collecting-mixin
                                 ;; when *do-print-log* is set, extensive event
                                 ;; log is printed
                                 http2::history-printing-object)
  ((end-stream-callback :accessor get-end-stream-callback :initarg :end-stream-callback
                        :documentation
                        "Callback to handle finished stream. Callled with one argument,
                         the stream. If result is true, terminate connection"))
  (:default-initargs :end-stream-callback (constantly nil)))

(defmethod peer-ends-http-stream ((connection vanilla-client-connection) stream)
  (when (funcall (get-end-stream-callback stream) stream)
    (terminate-locally connection)))

(defmethod initialize-instance :after ((connection vanilla-client-stream) &key &allow-other-keys)
  "Empty method to allow ignorable keys overflown from send-headers")
