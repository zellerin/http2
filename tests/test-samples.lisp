(in-package :http2/core)

(mgl-pax:defsection @test-samples
    (:title "Define test samples")
  (get-test-sample-code function))
;;;; Let us define few data samples that can be used to test an implementation -
;;;; either this one or remote one


(defvar *payloads* (make-hash-table))

(defun get-test-sample-code (name)
  (http2/core::payload-code (gethash name *payloads*)))

(defun get-first-line (text)
  (with-input-from-string (in text)
    (read-line in)))

(defstruct (payload (:print-object (lambda (object stream) (format stream "<Payload: ~a>" (get-first-line (payload-documentation object))))))
  (documentation "N/A" :type string)
  (code (make-octet-buffer 0) :type (or octet-vector compiled-function))
  (test (constantly nil) :type compiled-function))

(defmacro define-static-test-payload (name (&optional (connection-name))
                               &body body)
  `(setf (gethash ',name *payloads* )
         (make-payload
          :documentation ,(if (stringp (car body)) (pop body) "N/A")
          :code
          (lambda (conn)
            (declare (ignore conn))
            (let ,(when connection-name
                    `((,connection-name (make-instance 'http2/client:vanilla-client-connection
                                                       :network-stream (make-broadcast-stream)))))
              ,@body)))))

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

(define-static-test-payload end-of-file (conn)
  "Frame should have 10 octets, but has just 9"
  (write-frame conn -1 +data-frame+ (list :padded (make-octet-buffer 10))
               (constantly nil) #()))

(define-static-test-payload null-connection-window-update (conn)
  "Send empty update to a connection"
  (write-window-update-frame conn 0))

(define-static-test-payload even-numbered-stream (conn)
  "Send an even numbered stream. Server should not like it."
  (write-headers-frame
   (make-instance (get-stream-class conn)
                  :stream-id 2
                  :connection conn
                  :state 'open)
   (compile-headers (http2/client::request-headers "GET" "/" "localhost") nil)
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
               (progn (incf (get-id-to-use conn) 2)
                      (write-headers-frame (create-new-local-stream conn)
                                           (compile-headers (http2/client::request-headers "GET" "/" "localhost") nil)
                                           :end-headers t))
               (progn
                 (decf (get-id-to-use conn) 2)
                 (write-headers-frame
                  (create-new-local-stream conn)
                  (compile-headers (http2/client::request-headers "GET" "/" "localhost") nil)
                  :end-headers t))))

;; server side
(define-static-test-payload send-data-without-being-asked (conn)
  (write-data-frame (create-new-local-stream conn) (make-octet-buffer 0)))
