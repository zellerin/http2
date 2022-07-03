;;;; Copyright 2022 by Tomáš Zellerin

;;;; define VANILLA-CLIENT-CONNECTION with relatively sane client side
;;;; behaviour. Define WITH-HTTP-CONNECTION macro that allows to talk to other
;;;; ports.
(in-package :http2)

(defmacro with-http2-connection ((name class &rest params) &body body)
  "Run BODY with NAME bound to instance of CLASS with parameters.
Close the underlying network stream when done."
  `(let ((,name (make-instance ,class ,@params)))
     (unwind-protect
          (progn ,@body)
       (close ,name))))

(defun tls-connection-to (host &key (port 443) (sni host))
  (cl+ssl:make-ssl-client-stream
   (usocket:socket-stream
    (usocket:socket-connect host port))
   :verify nil
   :hostname sni
   :alpn-protocols '("h2")))

(defmacro with-http-connection ((connection target &key (sni target) (port 443)
                                          (connection-class ''vanilla-client-connection))
                                &body body)
  "Run BODY with established HTTP2 connection over TLS to PORT on
TARGET, using SNI."
  `(let ((network-stream  (tls-connection-to ,target :sni ,sni :port ,port)))
     (unless (equal (cl+ssl:get-selected-alpn-protocol network-stream) "h2")
       (close network-stream)
       (error "HTTP/2 not supported by ~a" ,sni))
     (with-http2-connection (connection ,connection-class
                                        :network-stream network-stream)
       (flet ((wait-for-responses ()
                (with-simple-restart (use-read-so-far "Use data read so far")
                  (handler-case
                      (loop
                        do
                           (read-frame ,connection)
                        until (and (get-finished ,connection)
                                   (null (listen (get-network-stream ,connection)))))
                    (end-of-file () nil)))))
         ,@body))))

(defun terminate-locally (connection &optional (code +no-error+))
  (write-goaway-frame connection connection 0 code #())
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

(defclass vanilla-client-io-connection (vanilla-client-connection)
  ()
  (:default-initargs :stream-class 'vanilla-client-io-stream))

(defclass vanilla-client-io-stream (client-stream ;; basic semantics and pseudoheaders
                                    ;; recieved data are stored in slot BODY
                                    data-frames-collecting-mixin
                                    ;; headers (not pseudoheaders) are collected in
                                    ;; slot HEADERS
                                    header-collecting-mixin
                                    ;; when *do-print-log* is set, extensive event
                                    ;; log is printed
                                    history-printing-object)
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

(defmethod initialize-instance :after ((connection vanilla-client-io-stream) &key &allow-other-keys)
  "Empty method to allow ignorable keys overflown from send-headers")
