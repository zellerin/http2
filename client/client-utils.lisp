;;;; Copyright 2022 by Tomáš Zellerin

;;;; define VANILLA-CLIENT-CONNECTION with relatively sane client side
;;;; behaviour. Define WITH-HTTP-CONNECTION macro that allows to talk to other
;;;; ports.
(in-package :http2)

(defun process-pending-frames (connection)
  "Read and process all queued frames."
  (handler-case
      (loop
        ;; make sure
        initially (force-output (get-network-stream connection))
        while (listen (get-network-stream connection))
        do (read-frame connection))
    (end-of-file () nil)))

(defmacro with-http2-connection ((name class &rest params) &body body)
  "Run BODY with NAME bound to instance of CLASS with parameters.
  Close the underlying network stream when done."
  `(let ((,name (make-instance ,class ,@params)))
     (unwind-protect
          (let ((values
                  (multiple-value-call 'list
                    (progn ,@body))))
            (write-goaway-frame ,name 0 +no-error+
                                (map 'vector 'char-code "All done"))
            (apply 'values values))
       (process-pending-frames ,name)
       (close ,name))))

(defun tls-connection-to (host &key (port 443) (sni host))
  "Client TLS stream to HOST on PORT, created using SNI and h2 ALPN protocol."
  (cl+ssl:make-ssl-client-stream
   (usocket:socket-stream
    (usocket:socket-connect host port))
   :verify nil
   :hostname sni
   :alpn-protocols '("h2")))

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
                                 http2::binary-output-stream-over-data-frames
                                 http2::binary-input-stream-over-data-frames
                                 http2::header-collecting-mixin
                                 http2::history-printing-object)
  ()
  (:documentation
   "Stream class for retrieve-url style functions. Behaves as a client stream,
   allows one to treat data frames as streams, collect headers to slot HEADERS
   so that they can be later shown as a list, and optionally prints callback
   logs. See individual superclasses for details."))

(defmethod initialize-instance :after ((connection vanilla-client-stream)
                                       &key &allow-other-keys)
  "Empty method to allow ignorable keys overflown from send-headers")

(defun make-transport-stream (raw-stream headers)
  "Make a transport stream from RAW-STREAM.

Guess encoding and need to gunzip from headers:
- apply zip decompression content-encoding is gzip (FIXME: also compression)
- use charset if understood in content-type
- otherwise guess whether text (use UTF-8) or binary."
  ;; This is POC level code. See Drakma on how to detect encoding more properly.
  (let* ((transport raw-stream)
         (content-type (cdr (assoc "content-type" headers :test 'equal)))
         (has-charset (search #1="charset=" content-type)))
    (when (member '("content-encoding" . "gzip") headers :test 'equalp)
      (setf transport (gzip-stream:make-gzip-input-stream transport)))
    (cond
      (has-charset
       (let ((charset (subseq content-type (+ (length #1#) has-charset))))
         (setf transport
               (flexi-streams:make-flexi-stream
                transport
                :external-format
                (cond
                  ((string-equal "UTF-8" charset) :utf8)
                  (t (warn "Unknown charset ~s, using UTF-8" charset)
                     :utf8))))))
      ((= 5 (mismatch "text/" content-type))
       (warn "Text without specified encoding, guessing utf-8")
       (setf transport
             (flexi-streams:make-flexi-stream
              transport
              :external-format :utf8)))
      ((= 7 (mismatch "binary/" content-type)))
      (t (warn "Content-type ~s not known to be text nor binary."
               content-type)))
    transport))
