;;;; Copyright 2022-2024 by Tomáš Zellerin

(in-package :http2)

(defsection @base-classes
    (:title "Classes")
  (http2-connection class)
  (flow-control-mixin class))

;;;; Classes
(defclass flow-control-mixin ()
  ((window-size      :accessor get-window-size      :initarg :window-size)
   (peer-window-size :accessor get-peer-window-size :initarg :peer-window-size))
  (:documentation
   "The flow control parameters that are kept both per-stream and per-connection."))

(defclass http2-connection (flow-control-mixin)
  ((streams                  :accessor get-streams                  :initarg :streams
                             :documentation "Sequence of HTTP2 streams")
   (acked-settings           :accessor get-acked-settings           :initarg :acked-settings)
   (compression-context      :accessor get-compression-context      :initarg :compression-context)
   (decompression-context    :accessor get-decompression-context    :initarg :decompression-context)
   (last-id-seen             :accessor get-last-id-seen             :initarg :last-id-seen
                             :type stream-id)
   (id-to-use                :accessor get-id-to-use                :initarg :id-to-use
                             :type stream-id)
   (stream-class             :accessor get-stream-class             :initarg :stream-class
                             :documentation "Class for new streams")
   (initial-window-size      :accessor get-initial-window-size      :initarg :initial-window-size)
   (initial-peer-window-size :accessor get-initial-peer-window-size :initarg :initial-peer-window-size)
   (max-frame-size           :accessor get-max-frame-size           :initarg :max-frame-size)
   (max-peer-frame-size      :accessor get-max-peer-frame-size      :initarg :max-peer-frame-size))
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
                     :max-frame-size 16384
                     :max-peer-frame-size 16384
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

(defgeneric get-state (state)
  (:method ((state (eql :closed))) 'closed)
  (:documentation
   "State of a HTTP stream. The parameter is either a HTTP2-STREAM object (that
keeps state in an appropriate slot) or placeholder values CLOSED or IDLE"))

(defclass http2-stream (flow-control-mixin)
  ((connection       :accessor get-connection       :initarg :connection)
   (stream-id        :accessor get-stream-id        :initarg :stream-id
                     :type stream-id)
   (state            :accessor get-state            :initarg :state
                     :type http2-stream-state)
   (data             :accessor get-data             :initarg :data)
   (weight           :accessor get-weight           :initarg :weight)
   (depends-on       :accessor get-depends-on       :initarg :depends-on)
   (seen-text-header :accessor get-seen-text-header :initarg :seen-text-header
                     :documentation
                     "Set if in the header block a non-pseudo header was already seen."))
  (:default-initargs :state 'idle :window-size 0
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
              :documentation
              "The path and query parts of the target URI"))
  (:default-initargs :method nil :scheme nil :authority nil :path nil))

(defmethod print-object ((stream server-stream) out)
  (if *print-escape*
      (print-unreadable-object (stream out :type t)
        (format out "#~d ~s ~s" (get-stream-id stream) (get-path stream) (get-state stream)))
      (format out (format out "Stream #~d ~s ~s" (get-stream-id stream) (get-path stream) (get-state stream)))))

(defclass log-headers-mixin ()
  ()
  (:documentation "Class that logs some activities and state changes."))

(defclass header-collecting-mixin ()
  ((headers :accessor get-headers :initarg :headers))
  (:default-initargs :headers nil)
  (:documentation
   "Mixin to be used to collect all observed headers to a slot."))

(defclass body-collecting-mixin ()
  ((body :accessor get-body :initarg :body))
  (:default-initargs :body "")
  (:documentation
   "Mixin to collect all payload parts to one string."))
;;;; Q: can one UTF-8 character be split into two frames?

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
  (queue-frame generic-function))

(defclass write-buffer-connection-mixin ()
  ((to-write :accessor get-to-write :initarg :to-write))
  (:default-initargs :to-write
   (make-array 3 :fill-pointer 0
                 :adjustable t))
  (:documentation
   "Stores queued frame in a per-connection write buffer. The internals of the
buffer are opaque."))

(defgeneric queue-frame (connection frame)
  (:method ((connection write-buffer-connection-mixin) frame)
    (vector-push-extend frame (get-to-write connection))
    frame))

;;;; Connection and stream for logging: tracks every callback in (reversed)
;;;; history.
(defclass logging-object ()
  ()
  (:documentation
   "Objects with this mixin have ADD-LOG called in many situations so that the
communication can be debugged or recorded."))

(defclass history-keeping-object (logging-object)
  ((reversed-history :accessor get-reversed-history :initarg :reversed-history))
  (:default-initargs :reversed-history nil))

(defclass history-printing-object (logging-object)
  ()
  (:documentation
   "A LOGGING-OBJECT that implements ADD-LOG to print all logs to
*TRACE-OUTPUT* as soon as it receives them."))

(defclass logging-connection (http2-connection history-keeping-object write-buffer-connection-mixin)
  ())

(defclass logging-stream (http2-stream history-keeping-object)
  ())


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
the first parameter.

In addition to the behaviour described below, all callback log the behaviour
when relevant stream or connection has logging-object as superclass."
  (@data-received section)
  (@stream-closed section)
  (apply-stream-priority  generic-function)
  (apply-window-size-increment generic-function)
  (peer-resets-stream generic-function)
  (set-peer-setting generic-function)
  (peer-expects-settings-ack generic-function)
  (peer-acks-settings generic-function)
  (handle-undefined-frame generic-function)
  (do-pong generic-function)
  (do-goaway generic-function))


#|


|#

(defun count-open-streams (connection)
  ;; 20240822 TODO: unused - remove or use to check when creating new stream.
  (count '(open half-closed/local half-closed/remote) (get-streams connection) :key #'get-state :test 'member))

(defmethod (setf get-state) :around (value (stream logging-object))
  (let ((before (get-state stream)))
    (add-log stream `(:state-change ,before -> ,value))
    (call-next-method))
  (call-next-method))

(defun open-http2-stream (connection headers &key end-stream (end-headers t) stream-pars)
  "Open http2 stream by sending headers."
  (send-headers (create-new-local-stream connection stream-pars)
                headers
                :end-stream end-stream
                :end-headers end-headers))

(defun send-headers (stream
                     headers &key end-stream (end-headers t)
                          &allow-other-keys)
  "Send HEADERS to a HTTP2 stream. The stream is returned.

The END-HEADERS and END-STREAM allow to set the appropriate flags."
  (with-slots (connection) stream
    (write-headers-frame stream
                         (compile-headers headers (get-compression-context connection))
                         :end-stream end-stream
                         :end-headers end-headers)
    (setf (get-updates-needed (get-compression-context connection)) nil))
  stream)

(defun peer-opens-http-stream-really-open (connection stream-id state)
  (unless (> stream-id (get-last-id-seen connection))
    (connection-error 'new-stream-id-too-low connection
                      :stream-id stream-id
                      :max-seen-so-far (get-last-id-seen connection)))
    ;; todo: count and check open streams
  (setf (get-last-id-seen connection) stream-id)
  (push (make-instance (get-stream-class connection)
                         :stream-id stream-id
                         :state state
                         :window-size (get-initial-window-size connection)
                         :peer-window-size (get-initial-peer-window-size connection)
                         :connection connection)
          (get-streams connection))
  (car (get-streams connection)))

(defgeneric is-our-stream-id (connection stream-id)
  (:documentation
   "Return true if the STREAM-ID should be initiated on our side. The ID is known
not to be zero.")
  (:method ((connection client-http2-connection) stream-id)
    (when (oddp stream-id) :even))

  (:method ((connection logging-connection) stream-id)
    (when (oddp stream-id) nil))

  (:method ((connection server-http2-connection) stream-id)
    (when (evenp stream-id) :odd)))

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
  (with-slots (connection state) stream
    (with-slots (streams) connection
      (setf streams (remove stream streams :test 'eq)
            state 'closed))))

(defgeneric peer-resets-stream (stream error-code)
  (:method ((stram (eql :closed)) error-code))
  (:method (stream error-code)
    (unwind-protect
         (unless (eq error-code +cancel+)
           (warn 'http-stream-error :stream stream :code error-code))
      (close-http2-stream stream)))
  (:method :before ((stream logging-object) error-code)
    (add-log stream  `(:closed :error ,(get-error-name error-code))))

  (:documentation
   "The RST_STREAM frame fully terminates the referenced stream and
   causes it to enter the \"closed\" state.  After receiving a RST_STREAM
   on a stream, the receiver MUST NOT send additional frames for that
   stream, with the exception of PRIORITY.  However, after sending the
   RST_STREAM, the sending endpoint MUST be prepared to receive and
   process additional frames sent on the stream that might have been
   sent by the peer prior to the arrival of the RST_STREAM."))

;;;; Other callbacks
(defgeneric maybe-lock-for-write (connection)
  (:method (connection) nil)
  (:documentation "This is called when a new frame is ready "))

(defgeneric maybe-unlock-for-write (connection)
  (:method (connection) nil)
  (:documentation "This is called when a new frame is ready "))

(defgeneric apply-data-frame (stream payload start end)
  (:documentation "HTTP-STREAM should process received PAYLOAD from the data frame. Presently it is called once per data frame, but this can change in future to improve performance.")

  ;; FIXME: we should not send small updates

  (:method (stream payload start end)
    "Just ignore the data and warn about it."
    (warn 'implement-by-user :format-control "No payload action defined."))

  (:method ((stream body-collecting-mixin) data start end)
    "Concatenate received data to the BODY slot of the object."
    (setf (get-body stream)
          (concatenate 'string (get-body stream)
                       (map 'string 'code-char (subseq data start end) )))
    (with-slots (connection) stream
      (write-window-update-frame connection (length data))
      (write-window-update-frame stream (length data))))

  (:method :before ((stream logging-object) payload start end)
    (add-log stream `(:payload ,(subseq payload start end)))))


(defgeneric apply-stream-priority (stream exclusive weight stream-dependency)
  (:method  (stream exclusive weight stream-dependency)
    (setf (get-weight stream) weight
          (get-depends-on stream)
          `(,(if exclusive :exclusive :non-exclusive) ,stream-dependency)))
  (:documentation
   "Called when priority frame - or other frame with priority settings set -
arrives. Does nothing, as priorities are deprecated in RFC9113 anyway.")

  (:method :before ((stream logging-object) exclusive weight stream-dependency)
    (add-log stream `(:new-prio :exclusive ,exclusive
                                :weight ,weight :dependency ,stream-dependency))))

(defgeneric apply-window-size-increment (object increment)
  (:documentation
   "Called on window update frame. By default, increases PEER-WINDOW-SIZE slot of
the strem or connection.")
  (:method ((object (eql :closed)) increment))
  (:method :before ((object logging-object) increment)
    (add-log object `(:window-size-increment ,(get-peer-window-size object) + ,increment)))

  (:method (object increment)
    (incf (get-peer-window-size object) increment)))

(defgeneric set-peer-setting (connection name value)
  (:documentation
   "Process received information about peers setting.

The setting relates to the CONNECTION. NAME is a keyword symbol (see
*SETTINGS*, subject to possible change to 16bit number in future) and VALUE is
32bit number.")
  (:method (connection name value)
    "Fallback."
    (declare (type (unsigned-byte 32) value))
    (warn 'unimplemented-feature :format-control "Peer settings not used - ~a ~a."
                                 :format-arguments (list name value)))

  (:method :before ((connection logging-object) name value)
    (declare (type (unsigned-byte 32) value))
    (add-log connection `(:setting ,name ,value)))

  (:method (connection (name (eql :header-table-size)) value)
    (declare (type (unsigned-byte 32) value))
    (let ((context (get-compression-context connection)))
      (when (< value (the (unsigned-byte 32) (get-dynamic-table-size context)))
        (update-dynamic-table-size context value))
      (push value (get-updates-needed context))))

  (:method (connection (name (eql :initial-window-size)) value)
    (declare (type (unsigned-byte 32) value))
    (if (> value (1- (expt 2 31)))
        (connection-error 'incorrect-initial-window-size-value connection
                          :value value)
        (setf (get-initial-peer-window-size connection) value)))

  (:method (connection (name (eql :max-frame-size)) value)
    (if (>= 16777215 value 16384)
        (setf (get-max-peer-frame-size connection) value)
        (connection-error 'incorrect-frame-size-value
         :value value)))

  (:method (connection (name (eql :no-rfc5740-priorities)) value)
    ;; we must signal if not 0 or 1. We MAY if it changes afterwards, so we do
    ;; not. (rfc9218)
    (unless (typep value 'bit)
      (connection-error 'incorrect-setting-value connection
                        :setting :no-rfc5740-priorities
                        :allowed 'bit
                        :value value)))

  (:method (connection (name (eql :max-header-list-size)) value)
    ;; This is just an advisory setting (10.5.1. Limits on Field Block Size) so
    ;; we ignore it for now
    (warn 'unimplemented-feature
          :format-control "We ignore :max-header-list-size. This is allowed in RFC9113."))

  (:method ((connection client-http2-connection) (name (eql :enable-push)) value)
    (declare (type (unsigned-byte 32) value))
    (unless (zerop value)
      (connection-error 'incorrect-enable-push-value connection
                        :value value)))

  (:method ((connection server-http2-connection) (name (eql :enable-push)) value)
    (declare (type (unsigned-byte 32) value))
    (if (> value 1)
        (connection-error 'incorrect-enable-push-value connection)
        (setf (get-peer-accepts-push connection) (plusp value))))

  (:method (connection (name (eql :max-concurrent-streams)) value)
    ;; do something
    ))

(defgeneric peer-expects-settings-ack (connection)
  (:documentation
   "Called when settings-frame without ACK is received, after individual
SET-PEER-SETTING calls. By default, send ACK frame.")

  (:method (connection)
    (write-ack-setting-frame connection)))


(defgeneric peer-acks-settings (connection)
  (:documentation
   "Called when SETTINGS-FRAME with ACK flag is received. By default does nothing.")
  (:method (connection)))


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
  (:method :after ((stream logging-object))
    (add-log stream '(:closed-remotely)))
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

(defgeneric add-header (connection stream name value)
  (:method :around (connection (stream http2-stream) name value)
    "Decode compressed headers"
    (let ((decoded-name
            (etypecase name
              ((or string symbol) name)
              ((vector (unsigned-byte 8)) (decode-huffman name)))))
      (when (and (stringp name) (some #'upper-case-p name))
        (http-stream-error 'lowercase-header-field-name stream))
      (call-next-method connection stream
                        decoded-name
                        (etypecase value
                          (string value) ; integer can be removed if we removed "200"
                          ((vector (unsigned-byte 8)) (decode-huffman value))))))

  (:method (connection stream name value)
    (warn 'implement-by-user
          :format-control "You should overwrite default method for adding new header."))

  (:method (connection (stream server-stream) (name symbol) value)
    (when (get-seen-text-header stream)
      (http-stream-error 'pseudo-header-after-text-header stream
                         :name name
                         :value value))
    (case name
      (:method
       (check-place-empty-and-set-it value get-method))
      (:scheme
       (check-place-empty-and-set-it value get-scheme))
      (:authority
       (check-place-empty-and-set-it value get-authority))
      (:path
       (check-place-empty-and-set-it value get-path))
      (t
       (http-stream-error 'incorrect-request-pseudo-header  stream
                          :name name :value value))))

  (:method (connection (stream client-stream) (name symbol) value)
    (when (get-seen-text-header stream)
      (http-stream-error 'pseudo-header-after-text-header stream
                         :name name
                         :value value))
    (case name
      (:status
          (setf (get-status stream) value))
      (t
       (http-stream-error 'incorrect-response-pseudo-header stream
                         :name name
                         :value value))))

  (:method :after (connection (stream log-headers-mixin) name value)
    (format t "~&header: ~a = ~a~%" name value))

  (:method (connection (stream header-collecting-mixin) name value)
    (push (cons name value) (get-headers stream)))

  (:method :before (connection (stream logging-object) name value)
    (add-log stream `(:header ,name ,value))))



(defgeneric send-ping (connection &optional payload)
  (:documentation
   "Send a ping request.")
  (:method (connection &optional (payload 0))
    (declare ((unsigned-byte 64) payload))
    (write-ping-frame connection payload))
  (:method ((connection timeshift-pinging-connection) &optional payload)
    (declare (ignore payload))
    (call-next-method connection (mod (get-internal-real-time) (expt 2 64)))))

(defgeneric process-end-headers (connection stream)
  (:method  :before (connection (stream logging-object))
    (add-log stream '(:end-headers)))

  (:method (connection stream))

  (:method (connection (stream client-stream))
    ;; Headers sanity check
    (unless (get-status stream)
      (http-stream-error 'missing-pseudo-header stream
                         :name :status
                         :value 'missing))
    ;; next header section may contain another :status
    (setf (get-seen-text-header stream) nil))

  (:method (connection (stream server-stream))
    ;; Headers sanity check
    (unless (or (equal (get-method stream) "CONNECT")
             (and (get-method stream) (get-scheme stream) (get-path stream)))
      (http-stream-error 'missing-pseudo-header stream
                         :name "One of pseudo headers"
                         :value 'missing))))

(defgeneric do-ping (connection data)
  (:documentation
   "Called when ping-frame without ACK is received. By default send ping-frame with
ACK and same data.")

  (:method (connection data)
    (write-ping-frame connection data :ack t))
  (:method :before ((connection logging-object) data)
    (add-log connection `(:ping ,data))))

(defgeneric do-pong (connection data)

  (:documentation
   "Called when ping-frame with ACK is received. By default warns about unexpected ping response; see also TIMESHIFT-PINGING-CONNECTION mixin.")
  (:method (connection data)
    (warn 'implement-by-user
          :format-control "The connection ~a did not expect ping response"
          :format-arguments (list connection)))
  (:method ((connection timeshift-pinging-connection) data)
    (format t "Ping time: ~5fs~%" (/ (- (get-internal-real-time) data) 1.0 internal-time-units-per-second))))

(defgeneric do-goaway (connection error-code last-stream-id debug-data)
  (:documentation
   "Called when a go-away frame is received. By default throws GO-AWAY condition if
error was reported.")
  (:method ((connection logging-object) error-code last-stream-id debug-data)
    (add-log connection `(:go-away :last-stream-id ,last-stream-id
                                   :error-code ,error-code)))

  (:method ((connection client-http2-connection) error-code last-stream-id debug-data)
    (unless (eq error-code '+no-error+)
      (error 'go-away :last-stream-id last-stream-id
                      :error-code error-code
                      :debug-data debug-data)))

  (:method ((connection server-http2-connection) error-code last-stream-id debug-data)
    (unless (eq error-code '+no-error+)
      (signal 'go-away :last-stream-id last-stream-id
                       :error-code error-code
                       :debug-data debug-data)
      (invoke-restart 'close-connection))))

(defgeneric handle-undefined-frame (connection type flags data)
  (:method (connection type flags data))
  (:documentation
   "Callback that is called when a frame of unknown type is received - see
extensions."))


(defvar *do-print-log* nil
  "Set to true value to log to the *TRACE-OUTPUT*.")

(defgeneric add-log (object log-pars)
  (:method ((object history-keeping-object) log-pars)
    (push log-pars (get-reversed-history object)))

  (:method ((object history-printing-object) log-pars)
    (when (or (eq *do-print-log* t)
              (member (car log-pars)  *do-print-log*))
      (let ((*print-length* 10)
            (*print-base* 16))
        (format *trace-output* "~&~s: ~{~s~^ ~}~%" object log-pars)))))

(defun get-history (object)
  (reverse (get-reversed-history object)))

(defvar +client-preface-start+
  #.(vector-from-hex-text "505249202a20485454502f322e300d0a0d0a534d0d0a0d0a")
  "The client connection preface starts with a sequence of 24 octets, which in hex notation is this. That is, the connection preface starts with the string
 \"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n\").")

(defun read-client-preface (connection)
  (let ((preface-buffer (make-octet-buffer (length +client-preface-start+))))
    (read-sequence preface-buffer (get-network-stream connection))
    (unless (equalp preface-buffer +client-preface-start+)
      (error 'client-preface-mismatch :received preface-buffer)))
  (write-settings-frame connection (get-settings connection)))

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

(defmethod do-pong :before ((connection logging-object) data)
  (add-log connection `(:pong ,data)))

;;;; network comm simplifications
(defmethod close ((connection http2-connection) &key &allow-other-keys)
  (break "This should not happen")
  (when (get-network-stream connection)
    (close (get-network-stream connection))))

(defgeneric handle-alt-svc (stream origin value)
  (:documentation
   "An ALTSVC frame from a server to a client on a stream other than
   stream 0 indicates that the conveyed alternative service is
   associated with the origin of that stream.

   An ALTSVC frame from a server to a client on stream 0 indicates that
   the conveyed alternative service is associated with the origin
   contained in the Origin field of the frame.  An association with an
   origin that the client does not consider authoritative for the
   current connection MUST be ignored.")

  (:method (stream origin value)
    "Default method ignores alt-svc info."
    (declare (ignore stream origin value)))

  (:method ((stream logging-object) origin value)
    (add-log stream `(:alt-svc ,origin ,value))))


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
