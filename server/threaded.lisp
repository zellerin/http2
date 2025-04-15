(in-package #:http2/server/threaded)

(defclass threaded-dispatcher (base-dispatcher)
  ()
  (:documentation
   "Specialize DO-NEW-CONNECTION to process new connections each in a separate thread. "))

(defclass tls-threaded-dispatcher (tls-dispatcher-mixin threaded-dispatcher)
  ())

(defclass detached-tls-threaded-dispatcher (detached-server-mixin tls-threaded-dispatcher)
  ())

(defclass detached-threaded-dispatcher (detached-server-mixin threaded-dispatcher)
  ())
(defclass detached-single-client-dispatcher (detached-server-mixin single-client-dispatcher)
  ())

(defmethod do-new-connection (listening-socket (dispatcher threaded-dispatcher))
  (let ((socket (usocket:socket-accept listening-socket
                                       :element-type '(unsigned-byte 8)))
        (context cl+ssl::*ssl-global-context*))
    (bt:make-thread
     (lambda ()
       (cl+ssl:with-global-context (context)
         (with-standard-handlers ()
           (restart-case
               (with-open-stream (stream (server-socket-stream socket dispatcher))
                 (http2/server::process-server-stream stream
                                                      :connection
                                                      (apply #'make-instance (get-connection-class dispatcher)
                                                             (get-connection-args dispatcher))))
             (kill-client-connection () nil))))) ; FIXME:
     ;; TODO: peer IP and port to name?
     :name "HTTP2 server thread for connection" )))

(defclass threaded-server-mixin ()
  ((scheduler :accessor get-scheduler :initarg :scheduler)
   (lock      :accessor get-lock      :initarg :lock))
  (:default-initargs
   :scheduler *scheduler*
   :lock (bt:make-lock))
  (:documentation
   "A mixin for a connection that holds a lock in actions that write to the output network
stream, and provides a second thread for scheduled activities (e.g., periodical
events)."))

(defmethod queue-frame :around ((server threaded-server-mixin) frame)
  (bt:with-lock-held ((get-lock server))
    (call-next-method)))

(defmethod cleanup-connection :after ((connection threaded-server-mixin))
    (stop-scheduler-in-thread (get-scheduler connection)))

(defmethod http2/server::server-socket-stream (socket (dispatcher tls-dispatcher-mixin))
  "The cl-ssl server socket."
  (with-slots (certificate-file private-key-file) dispatcher
    (cl+ssl:make-ssl-server-stream
     (call-next-method)
     :certificate certificate-file
     :key private-key-file)))

"For a TLS server wrap the global context."
(defmethod http2/server::start-server-on-socket ((server tls-threaded-dispatcher) socket)
  (cl+ssl:ensure-initialized)
  (cl+ssl:with-global-context ((make-http2-tls-context server) :auto-free-p t)
    (call-next-method)))

(define-condition not-http2-stream (serious-condition)
  ((tls-stream :accessor get-tls-stream :initarg :tls-stream)
   (alpn       :accessor get-alpn       :initarg :alpn))
  (:documentation
   "Signalled to decline handling of TLS stream as HTTP2 stream due to different ALPN.")
  (:report (lambda (condition stream)
             (format stream "The TLS stream ~A is not a HTTP2 stream (ALPN ~s)"
                     (get-tls-stream condition)
                     (get-alpn condition)))))
