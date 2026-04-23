(in-package http2/server)

(defsection @logging (:title "Server logging")
  "By default, vanilla servers log basic information. Current implementation logs
to the *STANDARD-OUTPUT* via *LOG-STREAM*. The specific output is subject to
change. For fixed output you can implement your own logging using provided
interface.
"
  (*log-stream* (variable '*standard-output*))
  (logging-connection-mixin class)
  (logging-stream-mixin class)
  (@logged-events section)
  (@logging-helpers section))


(defvar *log-stream* (make-synonym-stream '*standard-output*)
  "The stream used for generic logging. ")

(defsection @logged-events (:title "Logged events")
  "To reduce output clutter and possibly increase speed, you can filter out what is
logged by changing *LOGGED-EVENTS*. All values below are supposed to be opaque
and do not rely on integer interface."
  (*logged-events* variable)
  (+log-nothing+ variable)
  (+log-all+ variable)
  (set-log-event macro)
  "Following events are defined:"
  (+log-connect+ variable)
  (+log-disconnect+ variable)
  (+log-process-data+ variable)
  (+log-stream-closed+ variable))

(defconstant +log-all+ -1 "Value for *LOGGED-EVENTS* to log everything")
(defconstant +log-nothing+ 0 "Value for *LOGGED-EVENTS* to stop logging")

(defconstant +log-connect+ 0 "A client connects. Hooks to SETUP-CONNECTION.")
(defconstant +log-disconnect+ 1 "A client disconnects. Hooks to CLEANUP-CONNECTION.")
(defconstant +log-process-data+ 2 "Stream starts processing the request. Hooks to PEER-ENDS-HTTP-STREAM.")
(defconstant +log-stream-closed+ 3 "Stream is closed. Hooks to CLOSE-HTTP2-STREAM.")

(defun log-event (old event set)
  "Modified log event list to add or remove EVENT."
  (setf (ldb (byte 1 event) old) (if set 1 0))
  old)

(define-modify-macro set-log-event (event set) log-event
  "Modify PLACE to log (set is true) or stop logging (set is nil) an EVENT.
Presently, the PLACE would be *LOG-STREAM*, but in future there might be
per-dispatcher or per-connection values.

Example:
```
(set-log-event *logged-events* +log-disconnect+ nil)
```
stops logging disconnects.")

(defvar *logged-events*
  (let ((val 0))
    (set-log-event val +log-connect+ t)
    (set-log-event val +log-disconnect+ t)
    (set-log-event val +log-stream-closed+ nil)
    (set-log-event val +log-process-data+ t)
    val)
  "Specifies what events to log. Set it to +log-all+ or +log-nothing+, and use
SET-LOG-EVENT to fine tune what is logged.")

(defsection @logging-helpers (:title "Customize logging")
  "To customize logging, create you own stream and connection class, and specalize
relevant generic functions, filtering with WHEN-LOGGING."
  (when-logging macro))

(defmacro when-logging (event &body body)
  "Evaluate BODY when EVENT is supposed to be logged. E.g.,
```
(WHEN-LOGGING +LOG-CONNECT+ ...)
```"
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

(defmethod close-http2-stream :before ((stream logging-stream-mixin) error)
  (when-logging +log-stream-closed+
    (log-closed-stream stream error)))

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
