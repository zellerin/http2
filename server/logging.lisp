(in-package http2/server)

(defsection @logging (:title "Logging support")
  "Server should log some basic information.

Current implementation logs to the *STANDARD-OUTPUT* via *LOG-STREAM*. The
format is the peer name (see GET-PEER-NAME) and actual message.

What is logged:

- Client connected - see LOG-SERVER-CONNECTED, separate invocation from the
- Client disconnected"
  (*log-stream* variable)
  (*logged-events* variable))

(defvar *log-stream* (make-synonym-stream '*standard-output*)
  "The stream used for generic logging. ")

(defconstant +log-connect+ 0)
(defconstant +log-disconnect+ 1)
(defconstant +log-process-data+ 2)
(defconstant +log-stream-error+ 3)

(defun log-event (old event)
  (setf (ldb (byte 1 event) old) 1)
  old)

(define-modify-macro set-log-event (event) log-event)

(defvar *logged-events*
  (let ((val 0))
    (set-log-event val +log-connect+)
    (set-log-event val +log-disconnect+)
    (set-log-event val +log-stream-error+)
    (set-log-event val +log-process-data+)
    val))

(defmacro when-logging (event &body body)
  `(when (plusp (ldb (byte 1 ,event) *logged-events*))
     ,@body))

(defclass logging-stream-mixin ()
  ()
  (:documentation "Writes log information to the *log-stream* when finished."))

(defun log-closed-stream (stream e)
  "Log request information when peer sends all headers or when the request errs for
some reason..

This is to be bound to HTTP-STREAM-ERROR"
  (format *log-stream* "~&~A ~<~A~:> ~A"
          (get-peer-name (get-connection stream)) stream e)
  #+nil(format *log-stream* "~&~A ~@<~@[~A://~]~@[~A~]~@[~A~] [#~d] ~a ~:>~%"
          (get-peer-name (get-connection stream))
          (get-scheme stream)
          (get-authority stream)
          (get-path stream)
          (get-stream-id stream) e)
  (force-output *log-stream*))

(defmethod peer-ends-http-stream :before ((stream logging-stream-mixin))
  (when-logging +log-process-data+
    (log-closed-stream stream "- processing")))

(defclass logging-connection-mixin ()
  ()
  (:documentation "Writes log information to the *log-stream* on connection setup and close."))

(defmethod setup-connection :after ((connection logging-connection-mixin))
  "Log connection established."
  (when-logging +log-connect+
    (format *log-stream* "~&~A Connected, using ~A~%"
            (get-peer-name connection) connection)
    (force-output *log-stream*)))

(defmethod cleanup-connection :before ((connection logging-connection-mixin) &optional error)
  (when-logging +log-disconnect+
    (format *log-stream* "~&~A Disconnected ~a~%"
            (get-peer-name connection) error)
    (force-output *log-stream*)))
