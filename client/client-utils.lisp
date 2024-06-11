;;;; Copyright 2022, 2024 by Tomáš Zellerin

;;;; define VANILLA-CLIENT-CONNECTION with relatively sane client side
;;;; behaviour. Define WITH-HTTP-CONNECTION macro that allows to talk to other
;;;; ports.
(in-package :http2)

(mgl-pax:defsection @client
  (:title "Creating a client")
  (connect-to-tls-server function)
#+nil  (with-http2-connection macro)
  (send-headers function)
  (make-transport-output-stream function)
  (make-transport-input-stream function)
  (process-pending-frames function)
  (http-stream-to-vector function)
  (vanilla-client-stream class)
  (vanilla-client-connection class)
  (client-stream class)
  (header-collecting-mixin class)
  (client-http2-connection class)
  (extract-charset-from-content-type function))


(defun process-pending-frames (connection &optional just-pending)
  "Read and process frames on the input.

Finish normally when either

- peer closes connection (END-OF-FILE, CONNECTION-ERROR or SSL-ERROR was signalled), or
- JUST-PENDING was true, we are at a frame border and there is no additional input on the stream

This is to be called on client when the initial request was send, or on server
to serve requests.

May block."
  (handler-case
      (loop
        with frame-action = #'parse-frame-header
        and size = 9
        and stream = (get-network-stream connection)
        initially (force-output stream)
                  ;; Prevent ending when waiting for payload
        while (or (null just-pending)
                  (listen stream)
                  (not (eql #'parse-frame-header frame-action)))
        do
           (force-output stream)
           (let ((buffer (make-octet-buffer size)))
             (declare (dynamic-extent buffer))
             (if (= size (read-sequence buffer stream))
                 (multiple-value-setq
                     (frame-action size)
                     (funcall frame-action connection buffer))
                 (error 'end-of-file :stream (get-network-stream connection)))))
    (cl+ssl::ssl-error ()
      ;; peer may close connection and strange things happen
      (error 'end-of-file :stream (get-network-stream connection)))))

(defmacro with-http2-connection ((name class &rest params) &body body)
  "Run BODY with NAME bound to instance of CLASS with parameters.
  Close the underlying network stream when done."
  `(let ((,name (make-instance ,class ,@params)))
     (unwind-protect
          (unwind-protect
               (progn ,@body)
            (process-pending-frames ,name t))
       (handler-case
           (close ,name)
         (cl+ssl::ssl-error-zero-return ())))))

(defun connect-to-tls-server (host &key (port 443) (sni host) verify
                                 (alpn-protocols '("h2")))
  "Return a client TLS stream to HOST on PORT, created using SNI and with specified ALPN
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

(defmethod peer-ends-http-stream ((stream vanilla-client-stream))
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
  "Guess charset from the content type. NIL for binary data."
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
    ((alexandria:starts-with-subseq "text/" content-type)
     (warn "Text without specified encoding, guessing utf-8")
     *default-text-encoding*)
    ((alexandria:starts-with-subseq "binary/" content-type) nil)
    ;; see RFC8259. Note that there should not be charset in json CT
    ((string-equal content-type "application/json") :utf-8)
    (t (warn "Content-type ~s not known to be text nor binary. Using default ~a"
             content-type *default-encoding*)
       *default-encoding*)))

(defun make-transport-output-stream (http2-stream charset gzip)
  "An OUTPUT-STREAM built atop RAW STREAM with transformations based on HEADERS."
  (let* ((transport (make-instance 'payload-output-stream :base-http2-stream http2-stream)))
    (when gzip
      (setf transport (gzip-stream:make-gzip-output-stream transport)))
    (awhen charset
      (setf transport
            (flexi-streams:make-flexi-stream
             transport
             :external-format charset)))

    transport))

(defun make-transport-stream (http2-stream charset encoded)
  "INPUT-STREAM built atop a HTTP2-STREAM.

Guess encoding and need to gunzip from headers:
- apply zip decompression content-encoding is gzip (FIXME: also compression)
- use charset if understood in content-type
- otherwise guess whether text (use UTF-8) or binary."
  (let* ((transport (make-instance 'payload-input-stream :base-http2-stream http2-stream)))
    (when encoded
      (setf transport (gzip-stream:make-gzip-input-stream transport)))
    (when charset
      (setf transport
            (flexi-streams:make-flexi-stream transport :external-format charset)))
    transport))
