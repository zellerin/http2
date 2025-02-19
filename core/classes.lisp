;;;; Copyright 2022-2024 by Tomáš Zellerin

(in-package :http2/core)

(defsection @base-classes
    (:title "Classes")
  "There are two parallel class hierarchies, one for HTTP2 connections, one for the HTTP2 streams.

In general, CLIENT- and SERVER- classes implement required minimal behaviour for
the client, resp. server, and are supposed to be stable, while VANILLA classes
implement \"typical\" or best practice behaviour and may change in time. They
should be good for ad-hoc activities and experiments; for stable behaviour,
make your class using appropriate mixins.

![Class hierarchy](./classes.svg)"
  (http2-connection class)
  (client-http2-connection class)
  #+nil  (http2-stream class)
  (get-stream-class generic-function)
  (open-http2-stream function)
  (get-network-stream function)
  (server-stream class)
  (client-stream class)
  (close-connection restart)
  (get-body generic-function)
  (get-path generic-function)
  (send-headers generic-function)
  (get-method generic-function)
  (get-headers generic-function)
  (get-scheme generic-function)
  (get-authority generic-function)
  (get-status (method nil client-stream)))


;;;; Classes
(defclass http2-connection (frame-context stream-collection flow-control-mixin hpack-endpoint)
  ((acked-settings           :accessor get-acked-settings           :initarg :acked-settings)
   (stream-class             :accessor get-stream-class             :initarg :stream-class
                             :documentation "Class for new streams")
   (initial-window-size      :accessor get-initial-window-size      :initarg :initial-window-size)
   (initial-peer-window-size :accessor get-initial-peer-window-size :initarg :initial-peer-window-size))
  (:default-initargs :id-to-use 1
                     :last-id-seen 0
                     :streams nil
                     :acked-settings nil
                     :window-size 65535
                     :compression-context (make-instance 'hpack-context)
                     :decompression-context (make-instance 'hpack-context)
                     :stream-class 'http2-stream
                     :initial-peer-window-size 65535
                     :initial-window-size 65535
                     :peer-window-size 65535)

  (:documentation
   "A simple connection: promise push not allowed, otherwise reasonable behaviour"))

(defmethod print-object ((connection http2-connection) out)
  (print-unreadable-object (connection out :type t :identity nil)))

(defmethod get-stream-id ((c http2-connection))
  "It is useful sometimes to treat http connection as a stream with ID 0."
  0)

(defmethod get-connection ((c http2-connection))
  c)

(defclass client-http2-connection (http2-connection)
  ()
  (:default-initargs :id-to-use 1)
  (:documentation
   "Client connections have odd-numbered streams."))

(defclass server-http2-connection (http2-connection)
  ((peer-accepts-push :accessor get-peer-accepts-push :initarg :peer-accepts-push))
  (:default-initargs :id-to-use 2
   :peer-accepts-push t))

(defclass http2-stream (http2-stream-minimal flow-control-mixin)
  ((data             :accessor get-data             :initarg :data)
   (weight           :accessor get-weight           :initarg :weight)
   (depends-on       :accessor get-depends-on       :initarg :depends-on)
   (seen-text-header :accessor get-seen-text-header :initarg :seen-text-header
                     :documentation
                     "Set if in the header block a non-pseudo header was already seen."))
  (:default-initargs :window-size 0
   ;;   All streams are initially assigned a non-exclusive dependency on
   ;;   stream 0x0.  Pushed streams (Section 8.2) initially depend on their
   ;;   associated stream.  In both cases, streams are assigned a default
   ;;   weight of 16.
                     :weight 16
                     :depends-on '(:non-exclusive 0)
                     :seen-text-header nil)
  (:documentation
   "Representation of HTTP/2 stream. See RFC7540."))

(defgeneric get-network-stream (object)
  (:documentation "Get network stream for the object.")
  (:method ((object http2-stream))
    (get-network-stream (get-connection object))))

(defmethod initialize-instance :after ((stream http2-stream) &key connection)
  (with-slots (peer-window-size window-size) stream
    (unless  (slot-boundp stream 'peer-window-size)
      (setf peer-window-size (get-initial-peer-window-size connection)))
    (unless  (slot-boundp stream 'window-size)
      (setf window-size (get-initial-window-size connection)))))

(defclass client-stream (http2-stream)
  ((status :accessor get-status :initarg :status
           :documentation
           "HTTP status code field (see [RFC7231], Section 6)"))
  (:default-initargs :status nil)
  (:documentation
   "HTTP2 stream that checks headers as required for clients (no psedoheader other
than :status allowed, etc."))

(defclass server-stream (http2-stream)
  ((method    :accessor get-method    :initarg :method
              :documentation
              "The HTTP method ([RFC7231], Section 4)")
   (scheme    :accessor get-scheme    :initarg :scheme
              :documentation
              "Scheme portion of the target URI ([RFC3986], Section 3.1).

               Not restricted to \"http\" and \"https\" schemed URIs.
               A proxy or gateway can translate requests for non-HTTP schemes,
               enabling the use of HTTP to interact with non-HTTP services")
   (authority :accessor get-authority :initarg :authority
              :documentation
              "The authority portion of the target URI ([RFC3986], Section 3.2)")
   (path      :accessor get-path      :initarg :path
              :type string
              :documentation "The path and query parts of the target URI"))
  (:default-initargs :method nil :scheme nil :authority nil :path nil))

(defmethod print-object ((stream server-stream) out)
  (if *print-escape*
      (print-unreadable-object (stream out :type t)
        (format out "#~d ~s ~s" (get-stream-id stream) (get-path stream) (get-state stream)))
      (format out "Stream #~d ~s ~s" (get-stream-id stream) (get-path stream) (get-state stream))))

(defclass timeshift-pinging-connection ()
  ()
  (:documentation
   "A mixin that implements specific DO-PING and DO-PONG so that the RTT is printed
after DO-PING is send."))

;; 20240709 TODO: Link the section to documentation
(defsection @write-data-handling ()
  (:section "Writing frames")
  "There are two strategies how to write data that a connection produces - either
write them to a stream, or store them for future. They specialize QUEUE-FRAME
generic function to actually store the data."
  (write-buffer-connection-mixin class)
  (stream-based-connection-mixin class)
  (queue-frame generic-function)
  (parse-client-preface function)
  (+client-preface-start+ variable)
  (server-http2-connection class))


;;;; Callbacks from frame reading functions
(defsection @data-received
    (:title "Processing data frames")
  (apply-data-frame generic-function)
  (apply-data-frame (method nil (t t t t)))
  (body-collecting-mixin class)
  (apply-data-frame (method nil (body-collecting-mixin t t t))))


(defsection @stream-closed
    (:title "Processing end of data")
  (peer-ends-http-stream generic-function)
  (peer-ends-http-stream (method nil (vanilla-server-stream)))
  (peer-ends-http-stream (method nil (vanilla-client-stream))))

(defsection @callbacks
    (:title "Frame read callbacks")
  "The reader functions for individual frames may call a callback that is supposed
to handle received frame in some way. All callbacks have stream or connection as
the first parameter."
  (@data-received section)
  (@stream-closed section)
  (apply-stream-priority  generic-function)
  (apply-window-size-increment generic-function)
  (set-peer-setting generic-function)
  (peer-expects-settings-ack generic-function)
  (peer-acks-settings generic-function)
  (do-pong generic-function)
  (do-goaway generic-function))


#|


|#

(defun count-open-streams (connection)
  ;; 20240822 TODO: unused - remove or use to check when creating new stream.
  (count '(open half-closed/local half-closed/remote) (get-streams connection) :key #'get-state :test 'member))

(defun open-http2-stream (connection headers &key end-stream (end-headers t) stream-pars)
  "Open HTTP/2 stream (typically, from client side) by sending headers.

- STREAM-PARS are used as parameters for creating new stream instance.

- HEADERS are headers to be send for the client. You can use REQUEST-HEADERS to
  get necessary headers.

- END-HEADERS is aflag to the server that no more headers would be sent; true by default.

- END-STREAM is a flag to the server that there would be no payload."
  (send-headers (create-new-local-stream connection stream-pars)
                headers
                :end-stream end-stream
                :end-headers end-headers))

(defun send-headers (stream
                     headers &key end-stream (end-headers t)
                          &allow-other-keys)
  "Send HEADERS to a HTTP2 stream. The stream is returned.

The END-HEADERS and END-STREAM allow to set the appropriate flags.

Inside HANDLER macro, this names a function that has the STREAM
argument implicit and only HEADERS and key parameters are to be provided."
  (with-slots (connection) stream
    (write-simple-headers-frame stream
                         (compile-headers headers (get-compression-context connection))
                         :end-stream end-stream
                         :end-headers end-headers)
    (setf (get-updates-needed (get-compression-context connection)) nil))
  stream)

(defmethod is-our-stream-id ((connection client-http2-connection) stream-id)
  (when (oddp stream-id) :even))

(defmethod is-our-stream-id ((connection server-http2-connection) stream-id)
  (when (evenp stream-id) :odd))

(defgeneric peer-sends-push-promise (stream)
  (:method (stream) (error "Push promises not supported."))
  (:documentation
   "This should be called on push promise (FIXME: and maybe it is not, and maybe
the parameters should be different anyway). By default throws an error."))

(defun close-http2-stream (stream)
  "Close the http2 stream.

It marks the stream as closed, which is maybe unnecessary, as the stream is
immediately removed from the list of streams of its connection. This is
consistent with the concept that any stream not in the connection streams
explicitly is either idle (if above last-id-seen or id-to-use, depending on
even/odd) or closed - see FIND-HTTP-STREAM-BY-ID.

The removal of unused streams is necessary to prevent leakage for big requests -
other solution would be to send go-away after the number of streams is too high;
however some clients (e.g., h2load) do not retry when they receive this.

This stream removal should be done with lock on the appropriate stream when in
multiple threads."
  (with-slots (connection) stream
    (with-slots (streams) connection
      (setf streams (remove stream streams :test 'eq)
            (get-state stream) 'closed))))

;;;; Other callbacks
(defgeneric maybe-lock-for-write (connection)
  (:method (connection) nil)
  (:documentation "This is called when a new frame is ready "))

(defgeneric maybe-unlock-for-write (connection)
  (:method (connection) nil)
  (:documentation "This is called when a new frame is ready "))


(defun dynamic-table-entry-size (name value)
  "The size of an entry is the sum of its name's length in octets (as
   defined in Section 5.2), its value's length in octets, and 32.

   The size of an entry is calculated using the length of its name and
   value without any Huffman encoding applied."
  (+ 32 (length name) (length value)))

(defgeneric get-settings (connection)
  (:method-combination append)
  (:method append (connection)
    `((:max-frame-size . ,(get-max-peer-frame-size connection))
      (:header-table-size .
                          ,(get-dynamic-table-size (get-decompression-context connection)))))
  (:method append ((connection client-http2-connection))
    `((:enable-push . 0))))

(defmethod print-object ((o http2-stream) out)
  (if *print-escape*
      (print-unreadable-object (o out :type t :identity nil)
        (format out "~a #~d" (get-state o)
                (get-stream-id o)))
      (format out "~:(~a~) stream #~d" (get-state o) (get-stream-id o))))

(defgeneric peer-ends-http-stream (stream)
  (:documentation
   "Do relevant state changes when peer closes HTTP-STREAM (as part of received HEADERS or
PAYLOAD). Does nothing by default; client and server would want to specialize it to send response or process it.")
  (:method (stream)))

(defmacro check-place-empty-and-set-it (new-value accessor)
  "All HTTP/2 requests MUST include exactly one valid value for the
   :method, :scheme, and :path pseudo-header fields, unless it is
   a CONNECT request (Section 8.3).  An HTTP request that omits
   mandatory pseudo-header fields is malformed (Section 8.1.2.6)."
  ;; fixme: use place expanders
  `(if (,accessor stream)
       (http-stream-error 'duplicate-request-header stream
                          :name ',accessor
                          :value ,new-value)
       (setf (,accessor stream) ,new-value)))

(defgeneric send-ping (connection &optional payload)
  (:documentation
   "Send a ping request.")
  (:method (connection &optional (payload 0))
    (declare ((unsigned-byte 64) payload))
    (write-ping-frame connection payload))
  (:method ((connection timeshift-pinging-connection) &optional payload)
    (declare (ignore payload))
    (call-next-method connection (mod (get-internal-real-time) (expt 2 64)))))

(defmethod do-pong ((connection timeshift-pinging-connection) data)
  (format t "Ping time: ~5fs~%" (/ (- (get-internal-real-time) data) 1.0 internal-time-units-per-second)))

(defmethod do-goaway ((connection server-http2-connection) error-code last-stream-id debug-data)
  (unless (eq error-code '+no-error+)
    (signal 'go-away :last-stream-id last-stream-id
                     :error-code error-code
                     :debug-data debug-data)
    (invoke-restart 'close-connection)))


(defvar +client-preface-start+
  #.(vector-from-hex-text "505249202a20485454502f322e300d0a0d0a534d0d0a0d0a")
  "The client connection preface starts with a sequence of 24 octets, which in hex notation is this. That is, the connection preface starts with the string
 \"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n\").")

(defun parse-client-preface (connection buffer)
  "Parse client preface.

Check that buffer contains a client preface, or raise an error.

Then write the initial settings frame, and expect normal frame. Actually, this should be a settings frame, but this is not enforced now."
  (unless (equalp buffer +client-preface-start+)
    (error 'client-preface-mismatch :received buffer))
  (write-settings-frame connection (get-settings connection))
  (values #'parse-frame-header 9))

(defmethod initialize-instance :after ((connection client-http2-connection) &key &allow-other-keys)
  "In HTTP/2, each endpoint is required to send a connection preface as a
   final confirmation of the protocol in use and to establish the
   initial settings for the HTTP/2 connection.  The client and server
   each send a different connection preface.

   The client connection preface starts with a sequence of 24 octets.  This
   sequence MUST be followed by a SETTINGS frame (Section 6.5), which MAY be
   empty."
  ;; 20240708 TODO: This should go to client, or at least not depend on network stream
  (write-sequence +client-preface-start+ (get-network-stream connection))
  (write-settings-frame connection (get-settings connection)))


(defsection @old-frame-functions
    (:title "Read frames from Common Lisp streams")
  "This was the entry point for the version one of the library.

Reading from Common Lisp streams has problems with not being able to poll, as
well as some others I forgot."
  (read-frame function)
  (write-frame-header function))

(defun write-sequences (stream headers)
  "Write a tree of sequences to stream."
  (etypecase headers
    (null nil)
    (vector (write-sequence headers stream))
    (cons (map nil (lambda (a) (write-sequences stream a)) headers))))
