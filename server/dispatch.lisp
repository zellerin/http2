;;;; Copyright 2022 by Tomáš Zellerin

;;;; As an example of how could server be built, define class
;;;; vanilla-server-connection that dispatches incoming requests based on their
;;;; path to a handler function.  Handlers are mapped either to specific path
;;;; (exact handler) or to a prefix (prefix handler).
;;;;
;;;; use HANDLER macro to wrap handlers; it provides convenient and exported
;;;; function for the handler to use (SEND-DATA, SEND-TEXT, SEND-HEADERS,
;;;; SEND-GOAWAY).
;;;;
;;;; Of course, existing framework arount http/1.1 (Hunchentoot, ...) provide
;;;; much more. I would prefer not do duplicate.


(in-package :http2)

(defclass dispatcher-mixin ()
  ((exact-handlers  :accessor get-exact-handlers  :initarg :exact-handlers)
   (prefix-handlers :accessor get-prefix-handlers :initarg :prefix-handlers))
  (:default-initargs :exact-handlers nil :prefix-handlers nil)
  (:documentation
   "Keep two sets of handlers, exact and prefix. Run the handler to process the
request when peer closes the stream."))

(eval-when (:compile-toplevel :load-toplevel)
  (defun define-some-handler (target prefix fn)
    `(progn
       (setf ,target (remove ,prefix ,target :key 'car :test 'equal))
       (push (cons ,prefix ,fn) ,target))))

(defmacro handler ((flexi-stream-name &rest flexi-pars) &body body)
  `(lambda (connection stream)
     (with-open-stream (,flexi-stream-name (flexi-streams:make-flexi-stream
                                            stream
                             ,@flexi-pars))
       (flet ((send-headers (&rest args)
                (apply #'send-headers stream args))
              (send-goaway (code debug-data)
                (http2::write-goaway-frame connection
                                           0 code debug-data)
                (force-output (get-network-stream connection))))
         (declare (ignorable #'send-goaway))
         ,@body))))

(defmacro define-prefix-handler (prefix fn &optional connection)
  (define-some-handler (if connection
                           `(get-prefix-handlers connection) '*prefix-handlers*)
    prefix fn))

(defmacro define-exact-handler (prefix fn &optional connection)
  (define-some-handler (if connection
                           `(get-prefix-handlers connection) '*exact-handlers*)
    prefix fn))

(defvar *prefix-handlers*
  nil
  "Alist of prefixes and functions of connection and stream to make http response.")

(defvar *exact-handlers*
  ()
  "Alist of paths and functions of connection and stream to make http response.")

(defun send-text-handler (text &key (content-type "text/html")
                                 additional-headers)
  (handler (out)
    (send-headers `((:status "200") ("content-type" ,content-type)
                    ,@additional-headers))
    (princ text out)))

(defun redirect-handler (target &key (code "301") (content-type "text/html") content)
  (handler (out)
    (send-headers `((:status ,code)
                    ("location" ,target)
                    ,@(when content `(("content-type" ,content-type))))
                  :end-stream (null content))
    (when content
      (princ content  out))))

;;;; Sample server with constant payload
(defclass vanilla-server-connection (server-http2-connection
                                    dispatcher-mixin
                                    history-printing-object)
  ()
  (:default-initargs :stream-class 'vanilla-server-stream))

(defclass vanilla-server-stream (server-stream
                                 binary-output-stream-over-data-frames
                                 body-collecting-mixin
                                 history-printing-object)
  ())

(defmethod peer-ends-http-stream ((stream vanilla-server-stream))
  "Send some random payloads, or shut down the server."
  (let ((handler
            (or
             (cdr (assoc (get-path stream) *exact-handlers*
                         :test (lambda (prefix path)
                                 (let ((mismatch (mismatch prefix path)))
                                   (or (null mismatch)
                                       (and (eql mismatch (position #\? path))
                                            (eql mismatch (length path))))))))
             (cdr (assoc (get-path stream) *prefix-handlers*
                         :test (lambda (prefix path)
                                 (let ((mismatch (mismatch prefix path)))
                                   (or (null mismatch) (equal mismatch (length path))))))))))
    (with-slots (connection) stream
      (if handler (funcall handler connection stream)
          (progn
            (write-headers-frame stream `((:status "404") ("content-type" "text/html")) :end-headers t)
            (write-data-frame stream (map 'vector 'char-code "<h1>Not found</h1>")
                              :end-stream t))))))

(defun process-server-stream (stream &key (connection-class 'vanilla-server-connection))
  (let ((connection (make-instance connection-class
                                   :network-stream stream)))
    (with-simple-restart (close-connection "Close current connection")
      (handler-case
          (loop (read-frame connection))
        (end-of-file () nil)
        (go-away ())))))
