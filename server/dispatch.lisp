(in-package http2)

(defclass dispatcher-mixin ()
  ((exact-handlers  :accessor get-exact-handlers  :initarg :exact-handlers)
   (prefix-handlers :accessor get-prefix-handlers :initarg :prefix-handlers))
  (:default-initargs :exact-handlers nil :prefix-handlers nil)
  (:documentation
   "Keep two sets of handlers, exact and prefix. Run the handler to process the
request when peer closes the stream."))

(eval-when (:compile-toplevel :load-toplevel)
  (defun define-some-handler (target prefix fn)
    `(push (cons ,prefix ,fn)
           ,target)))

(defmacro handler (&body body)
  `(lambda (connection stream)
     (flet ((send-headers (&rest args)
              (apply #'send-headers connection stream args))
            (send-data (&rest args)
              (apply #'write-data-frame connection stream args))
            (send-text (text &rest args)
              (apply #'write-data-frame connection stream
                     (map 'vector 'char-code text)
                    args)))
       (declare (ignorable #'send-data #'send-text))
       ,@body
       (force-output (get-network-stream connection)))))

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
  (handler
    (send-headers `((:status "200") ("content-type" ,content-type)
                    ,@additional-headers))
    (send-text text :end-stream t)))

(defun redirect-handler (target &key (code "301") (content-type "text/html") content)
  (handler
    (send-headers `((:status ,code)
                    ("location" ,target)
                    ,@(when content `(("content-type" ,content-type))))
                  :end-stream (null content))
    (when content
      (send-text content :end-stream t))))
