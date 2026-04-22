(in-package http2/server)

(defsection @logging (:title "Logging support")
  "Server should log some basic information.

Current implementation logs to the *STANDARD-OUTPUT* via *LOG-STREAM*. The
format is the peer name (see GET-PEER-NAME) and actual message.

What is logged:

- Client connected - see LOG-SERVER-CONNECTED, separate invocation from the
- Client disconnected"
  (*log-stream* variable))

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
  (log-closed-stream stream "- processing"))

(defclass logging-connection-mixin ()
  ()
  (:documentation "Writes log information to the *log-stream* on connection setup and close."))

(defmethod setup-connection :after ((connection logging-connection-mixin))
  "Log connection established."
  (format *log-stream* "~&~A Connected, using ~A~%"
          (get-peer-name connection) connection)
  (force-output *log-stream*))

(defmethod cleanup-connection :before ((connection logging-connection-mixin) &optional error)
  (format *log-stream* "~&~A Disconnected ~a~%"
          (get-peer-name connection) error)
  (force-output *log-stream*))
