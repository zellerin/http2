;;;; Copyright 2022 by Tomáš Zellerin

;;;; Create a specific server. Use dispatch handlers to define behaviour of the
;;;; server, and actually bind it to a TLS socket.

(defpackage :http2/server
  (:use :cl :http2)
  (:export #:create-server))

(in-package :http2/server)

(define-prefix-handler "/re" (redirect-handler "/ok"))

(define-exact-handler "/ok" (send-text-handler "Redirect was OK"
                                               :content-type "text/plain"
                                               :additional-headers '(("refresh" "3; url=/"))))

(defvar *intro-page*
  "<h1>Hello World</h1>
<p>This server is for testing http2 protocol</p>
<p><a href='/redir'>Redirect test</a>
<form action='/body' method='post'><input type='submit' name='xxx' value='POST query test'></form")

(define-exact-handler "/" (send-text-handler *intro-page*))

(define-exact-handler "/body"
    (handler
      (send-headers `((:status "200") ("content-type" "text/plain")
                      ("refresh" "3; url=/")))
      (send-text (get-body stream) :end-stream t)))

#+sbcl (define-exact-handler "/exit"
  (handler
    (send-headers `((:status "200")))
    (send-text "Goodbye" :end-stream t)
    (send-goaway +no-error+ #())
    (sb-ext:quit)))

#+sbcl (defun sb-threadify (fn &rest args)
         (sb-thread:make-thread fn :arguments args))

(defvar *dispatch-fn* #'funcall
  "How to call process-server-stream. Default is funcall, or sb-thredify on sbcl.")

(defun wrap-to-tls-and-process-server-stream (raw-stream key cert)
  (unwind-protect
       (handler-case
           (let ((tls-stream (cl+ssl:make-ssl-server-stream
                              raw-stream
                              :certificate cert
                              :key key)))
             (if (equal "h2" (cl+ssl:get-selected-alpn-protocol tls-stream))
                 (funcall *dispatch-fn*
                          #'process-server-stream  tls-stream)))
         (error (err)
           (describe err)))
    (close raw-stream)))

(defun make-http2-tls-context ()
  (let ((context
          (cl+ssl:make-context
           ;; Implementations of HTTP/2 MUST use TLS
           ;; version 1.2 [TLS12] or higher for HTTP/2
           ;; over TLS.
           :min-proto-version cl+ssl::+TLS1-2-VERSION+

           ;;   A deployment of HTTP/2 over TLS 1.2 MUST disable compression.  TLS
           ;;   compression can lead to the exposure of information that would not
           ;;   otherwise be revealed [RFC3749].  Generic compression is unnecessary
           ;;   since HTTP/2 provides compression features that are more aware of
           ;;   context and therefore likely to be more appropriate for use for
           ;;   performance, security, or other reasons.
           :options (list #x20000 ; +ssl-op-no-compression+
                          cl+ssl::+ssl-op-all+
                          #x40000000) ; no renegotiation
           ;; do not requiest client cert
           :verify-mode cl+ssl:+ssl-verify-none+)))
    (cl+ssl::ssl-ctx-set-alpn-select-cb
     context
     (cffi:get-callback 'cl+ssl::select-h2-callback))
    context))

(defun create-server (port key cert &key ((:verbose http2::*do-print-log*))
                                      (announce-open-fn (constantly nil)))
  (usocket:with-server-socket (socket (usocket:socket-listen "127.0.0.1" port
                                                             :reuse-address t
                                                             :backlog 200))
    (cl+ssl:with-global-context ((make-http2-tls-context) :auto-free-p t)
      (funcall announce-open-fn)
      (loop
        (wrap-to-tls-and-process-server-stream
         (usocket:socket-stream
          (handler-case
              (usocket:socket-accept socket  :element-type '(unsigned-byte 8))
            ;; ignore condition
            (usocket:connection-aborted-error ())))
         key cert)))))

(defun main ()
  (handler-bind ((warning 'muffle-warning))
    (create-server 1230 "/tmp/server.key" "/tmp/server.crt"
                   :verbose nil)))

;;;;
;;;; curl: send settings and wait for settings.
