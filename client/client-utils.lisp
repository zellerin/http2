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
  (process-pending-frames function)
  (http-stream-to-vector function)
  (vanilla-client-stream class)
  (vanilla-client-connection class)
  (client-stream class)
  (finish-stream restart)
  (header-collecting-mixin class)
  (client-http2-connection class)
  (extract-charset-from-content-type function))


(defmacro with-http2-connection ((name class &rest params) &body body)
  "Run BODY with NAME bound to instance of CLASS with parameters."
  `(let ((,name (make-instance ,class ,@params)))
     (unwind-protect
          (progn ,@body)
       (process-pending-frames ,name t))))

(defun connect-to-tls-server (host &key (port 443) (sni host) verify
                                 (alpn-protocols '("h2")))
  "Return a client TLS stream to HOST on PORT, created using SNI and with specified ALPN
protocol (H2 by default)."
  (let ((stream (cl+ssl:make-ssl-client-stream
                 (usocket:socket-stream
                  (usocket:socket-connect host port :element-type '(unsigned-byte 8)))
                 :verify verify :hostname sni :alpn-protocols alpn-protocols)))
    (unless (equal "h2" (cl+ssl:get-selected-alpn-protocol stream))
      (error 'h2-not-supported-by-server :host host :port port))
    stream))

(defclass vanilla-client-connection (client-http2-connection
                                     stream-based-connection-mixin
                                     http2::history-printing-object
                                     http2::timeshift-pinging-connection)
  ()
  (:default-initargs :stream-class 'vanilla-client-stream)
  (:documentation
   "Connection class for retrieve-url style functions that uses streams of
   VANILLA-CLIENT-STREAM. Behaves as client, can send pings to measure roundtrip
   time and optionally prints history. See individual superclasses for details."))

(defclass text-collecting-stream ()
  ((text :accessor get-text :initarg :text))
  (:default-initargs :text nil)
  (:documentation
   "Mixin that collect all the received body (possibly unzipped data frames
converted to proper encoding) into a TEXT slot."))

(defclass vanilla-client-stream (http2::utf8-parser-mixin
                                 http2::gzip-decoding-mixin
                                 client-stream
                                 http2::header-collecting-mixin
                                 http2::history-printing-object
                                 text-collecting-stream)
  ((end-headers-fn :accessor get-end-headers-fn :initarg :end-headers-fn))
  (:default-initargs :end-headers-fn (constantly nil))
  (:documentation
   "Stream class for retrieve-url style functions. Behaves as a client stream,
   allows one to treat data frames as streams, collect headers to slot HEADERS
   so that they can be later shown as a list, and optionally prints callback
   logs. See individual superclasses for details."))


(defmethod process-end-headers :after (connection (stream vanilla-client-stream))
  (funcall (get-end-headers-fn stream) stream))

(mgl-pax:define-restart finish-stream (http2-stream)
  "Invoked when server fully sends the response to the VANILLA-CLIENT-STREAM.")

(defmethod peer-ends-http-stream ((stream vanilla-client-stream))
  (invoke-restart 'finish-stream stream))

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
