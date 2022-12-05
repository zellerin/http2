;;;; Copyright 2022 by Tomáš Zellerin

;;;; define VANILLA-CLIENT-CONNECTION with relatively sane client side
;;;; behaviour. Define WITH-HTTP-CONNECTION macro that allows to talk to other
;;;; ports.
(in-package :http2)

(defun process-pending-frames (connection &optional just-pending)
  "Read and process all queued frames. This is to be called on client when the
initial request was send.

See PROCESS-SERVER-STREAM in dispatch.lisp for a server equivalent that reads
all; note that that one also handles locking in multithread environment and some
other conditions."
  (handler-case
      (loop
        initially (force-output (get-network-stream connection))
        while (or (null just-pending) (listen (get-network-stream connection)))
        do (read-frame connection))
    (end-of-file () nil)))

(defmacro with-http2-connection ((name class &rest params) &body body)
  "Run BODY with NAME bound to instance of CLASS with parameters.
  Close the underlying network stream when done."
  `(let ((,name (make-instance ,class ,@params)))
     (unwind-protect
          (progn ,@body)
       (process-pending-frames ,name t)
       (close ,name))))

(defun connect-to-tls-server (host &key (port 443) (sni host) verify
                                 (alpn-protocols '("h2")))
  "Client TLS stream to HOST on PORT, created using SNI and with specified ALPN
protocol (H2 by default)."
  (cl+ssl:make-ssl-client-stream
   (usocket:socket-stream
    (usocket:socket-connect host port :element-type '(unsigned-byte 8)))
   :verify verify :hostname sni :alpn-protocols alpn-protocols))

(defclass vanilla-client-connection (client-http2-connection
                                     http2::history-printing-object
                                     http2::timeshift-pinging-connection)
  ()
  (:default-initargs :stream-class 'vanilla-client-stream)
  (:documentation
   "Connection class for retrieve-url style functions that uses streams of
   VANILLA-CLIENT-STREAM. Behaves as client, can send pings to measure roundtrip
   time and optionally prints history. See individual superclasses for details."))

(defclass vanilla-client-stream (client-stream
                                 http2-stream-with-input-stream
                                 http2::header-collecting-mixin
                                 http2::history-printing-object)
  ((end-headers-fn :accessor get-end-headers-fn :initarg :end-headers-fn)
   (end-stream-fn  :accessor get-end-stream-fn  :initarg :end-stream-fn))
  (:default-initargs :end-stream-fn (constantly nil)
                     :end-headers-fn (constantly nil))
  (:documentation
   "Stream class for retrieve-url style functions. Behaves as a client stream,
   allows one to treat data frames as streams, collect headers to slot HEADERS
   so that they can be later shown as a list, and optionally prints callback
   logs. See individual superclasses for details."))

(defmethod process-end-headers :after (connection (stream vanilla-client-stream))
  (funcall (get-end-headers-fn stream) stream))

(defmethod peer-ends-http-stream :after ((stream vanilla-client-stream))
  (funcall (get-end-stream-fn stream) stream))
