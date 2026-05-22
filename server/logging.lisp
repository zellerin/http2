(in-package http2/server)

(defsection @logging (:title "Logging support")
  "Server should log some basic information.

Current implementation logs to the *STANDARD-OUTPUT* via *LOG-STREAM*. The
format is the peer name (see GET-PEER-NAME) and actual message.

What is logged:

- Client connected - see LOG-SERVER-CONNECTED, separate invocation from the
- Client disconnected"
  (*log-stream* variable))

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
