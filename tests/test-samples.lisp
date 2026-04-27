(in-package :http2/tests/frames)

(mgl-pax:defsection @test-samples
    (:title "Define test samples")
  (get-test-sample-code function))
;;;; Let us define few data samples that can be used to test an implementation -
;;;; either this one or remote one

(defvar *payloads* (make-hash-table))

(defstruct (payload
            (:print-object (lambda (object stream)
                             (format stream "<Payload: ~a>"
                                     (get-first-line (payload-documentation object))))))
  "Represents a test to be perfomed on a peer and expected error."
  (documentation "N/A" :type string)
  (code (make-octet-buffer 0) :type (or octet-vector compiled-function))
  (test (constantly nil) :type compiled-function))

(defun get-test-sample-code (name)
  (http2/core::payload-code (gethash name *payloads*)))

(defun get-first-line (text)
  (with-input-from-string (in text)
    (read-line in)))

(defclass dummy-client-connection (client-http2-connection dummy-connection)
  ())

(defclass dummy-server-connection (server-http2-connection dummy-connection)
  ())

(defun run-maybe-with-dummy-client-connection (fn)
  "Apply FN on a dummy client connection, and return what would be written to the
underlying connection, without client preface and options."
  (let ((connection (make-instance 'dummy-client-connection)))
    (setf (fill-pointer (get-to-write connection)) 0)
    (funcall fn connection)
    (get-to-write connection)))

(defmacro define-static-test-payload (name (&optional (connection-name) (error-name name))
                                      &body body)
  "Register a PAYLOAD object with static payload (an octet vector).

If CONNECTION-NAME is set, it would be bound to a client stream during
evaluation, and payload is what the BODY writes to it.

If CONNECTION-NAME is NIL, payload is value of the last form in BODY."
  `(setf (gethash ',name *payloads* )
         (make-payload
          :documentation ,(if (stringp (car body)) (pop body) "N/A")
          :test (lambda () (http2/core::test-for-error ',name ',error-name))
          :code
          (lambda (conn)
            (declare (ignore conn))
            ,(if connection-name
                 `(run-maybe-with-dummy-client-connection
                   (lambda (,connection-name) ,@body))
                 `(progn ,@body))))))

(defmacro define-test-payload (name (&optional (connection-name))
                               &body body)
  `(setf (gethash ',name *payloads* )
         (make-payload
          :documentation ,(if (stringp (car body)) (pop body) "N/A")
          :code
          (lambda (,connection-name)
            ,@body))))

;;;; Payloads itself
(define-static-test-payload too-big-padding ()
  "Padding leaves no space for even empty real content. This should fail both on client and on the server."
  (http2/utils:make-initialized-octet-buffer #(0 0 10 1 8 0 0 0 1 10 0 0 0 0 0 0 0 0 0)))

(define-static-test-payload end-of-file (conn frame-type-needs-stream)
  "Frame should have 10 octets, but has just 9"
  (http2/core::write-frame conn -1 http2/core::+data-frame+ (list :padded (make-octet-buffer 10))
               (constantly nil) #()))

(define-static-test-payload null-connection-window-update (conn)
  "Send empty update to a connection"
  (write-window-update-frame conn 0))

(define-static-test-payload even-numbered-stream (conn our-id-created-by-peer)
  "Send an even numbered stream. Server should not like it."
  (write-headers-frame
   (make-instance (get-stream-class conn)
                  :stream-id 2
                  :connection conn
                  :state 'open)
   (http2/hpack:compile-headers (http2/client::request-headers "GET" "/" "localhost") nil)
   :end-headers t))


;;;; Payloads after an existing stream is created
(defmacro with-new-stream ((stream-name) &body body)
  `(let ((,stream-name (create-new-local-stream conn)))
     (concatenate 'octet-vector
                  (write-headers-frame ,stream-name
                                       (compile-headers (http2/client::request-headers "GET" "/" "localhost") nil)
                                       :end-headers t)
                  ,@body)))

(define-static-test-payload null-stream-window-update (conn)
  "Send empty update to a stream"
  (with-new-stream (new-stream)
    (write-window-update-frame new-stream 0)))

(define-test-payload id-one-after-id-3 (conn)
  (concatenate 'octet-vector
               (progn (incf (http2/core::get-id-to-use conn) 2)
                      (write-headers-frame (create-new-local-stream conn)
                                           (compile-headers (http2/client::request-headers "GET" "/" "localhost") nil)
                                           :end-headers t))
               (progn
                 (decf (http2/core::get-id-to-use conn) 2)
                 (write-headers-frame
                  (create-new-local-stream conn)
                  (compile-headers (http2/client::request-headers "GET" "/" "localhost") nil)
                  :end-headers t))))

;; server side
(define-static-test-payload send-data-without-being-asked (conn BAD-STREAM-STATE)
  (write-data-frame (create-new-local-stream conn) (make-octet-buffer 0)))

(define-static-test-payload frame-type-needs-stream (conn)
  (write-headers-frame
   conn
   (compile-headers (http2/client:request-headers "GET" "/" "localhost") nil)
     :end-headers t))

(define-static-test-payload frame-type-needs-connection (conn)
  (with-new-stream (stream)
    (write-settings-frame stream nil)))
