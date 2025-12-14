(in-package http2/server)

(defsection @logging (:title "Logging support")
  "Server should log some basic information.

Current implementation logs to the *STANDARD-OUTPUT* via *LOG-STREAM*. The
format is the peer name (see GET-PEER-NAME) and actual message.

What is logged:

- Client connected - see LOG-SERVER-CONNECTED, separate invocation from the
- Client disconnected"
  (*log-stream* variable))

(defvar *log-stream* (make-synonym-stream '*standard-output*))

(defgeneric log-server-connected (connection)
  (:documentation "Log connection established (with PEER).")
  (:method (connection)
    (format *log-stream* "~&~A Connected, using ~A~%"
            (get-peer-name connection) connection)
    (force-output *log-stream*)))

(defgeneric log-server-disconnected (connection error)
  (:documentation "Connection ends")
  (:method (connection error)
    (format *log-stream* "~&~A Disconnected ~a~%"
            (get-peer-name connection) error)
    (force-output *log-stream*)))

(defun log-closed-stream (stream e)
  "Log request information when peer sends all headers or when the request errs for
some reason..

This is to be bound to HTTP-STREAM-ERROR"
  (format *log-stream* "~&~A ~@<~A [#~d] ~a ~:>~%"
          (http2/server::get-peer-name (get-connection stream)) (get-path stream)
          (http2/core::get-stream-id stream) e)
  (force-output *log-stream*))
