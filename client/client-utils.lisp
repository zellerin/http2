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

(defvar *charset-names*
  '(("UTF-8" . :utf-8))
  "Translation table from header charset names to FLEXI-STREAM keywords.")

(defvar *default-encoding* nil
  "Character encoding to be used when not recognized from headers. Default is nil
- binary.")

(defvar *default-text-encoding* :utf8
  "Character encoding for text/ content to be used when not recognized from headers.")

(defun extract-charset-from-content-type (content-type)
  "Guess charset from the content type. NIL for binary data.

This is not designed to hat some content types (such as application/json) have defined
encoding (UTF-8)"
  (acond
    ((null content-type)
     (warn "No content type specified, using ~a" *default-encoding*)
     *default-encoding*)
    ((search #1="charset=" content-type)
     (let ((header-charset (subseq content-type (+ (length #1#) it))))
       (or (cdr (assoc header-charset *charset-names* :test 'string-equal))
           (warn "Unrecognized charset ~s, using default ~a" header-charset
                 *default-text-encoding*)
           *default-text-encoding*)))
    ((= 5 (mismatch "text/" content-type))
     (warn "Text without specified encoding, guessing utf-8")
     *default-text-encoding*)
    ((= 7 (mismatch "binary/" content-type)) nil)
    (t (warn "Content-type ~s not known to be text nor binary. Using default ~a"
             content-type *default-encoding*)
       *default-encoding*)))


(defun make-transport-output-stream (raw-stream headers)
  "An OUTPUT-STREAM built atop RAW STREAM with transformations based on HEADERS."
  (let* ((transport raw-stream)
         (charset (extract-charset-from-content-type
                   (cdr (assoc "content-type" headers :test 'equal)))))
    (when (member '("content-encoding" . "gzip") headers :test 'equalp)
      (setf transport (gzip-stream:make-gzip-output-stream transport)))
    (awhen charset
      (setf transport
            (flexi-streams:make-flexi-stream
             transport
             :external-format it)))

    transport))

(defun make-transport-stream (raw-stream headers)
  "Make a transport output stream from RAW-STREAM.

Guess encoding and need to gunzip from headers:
- apply zip decompression content-encoding is gzip (FIXME: also compression)
- use charset if understood in content-type
- otherwise guess whether text (use UTF-8) or binary."
  ;; This is POC level code. See Drakma on how to detect encoding more properly.
  (let* ((transport raw-stream)
         (charset (extract-charset-from-content-type
                   (cdr (assoc "content-type" headers :test 'equal)))))
    (when (member '("content-encoding" . "gzip") headers :test 'equalp)
      (setf transport (gzip-stream:make-gzip-input-stream transport)))
    (awhen charset
      (setf transport
            (flexi-streams:make-flexi-stream transport :external-format charset)))
    transport))
