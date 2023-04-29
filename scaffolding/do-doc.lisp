(load "~/quicklisp/setup")
(ql:quickload 'cz.zellerin.doc)
(ql:quickload 'http2/all)

(in-package http2)
(import '(cz.zellerin.doc:define-section
          cz.zellerin.doc:export-pkg-to-org))

(setf (getf cz.zellerin.doc:*package-sections* 'cz.zellerin.doc::http2)
      '(@client @api-frames @callbacks @new-frames-api))

(cz.zellerin.doc::define-section @client
  ""
  (connect-to-tls-server)
  (with-http2-connection)
  (send-headers)
  (make-transport-output-stream)
  (make-transport-input-stream)
  (process-pending-frames)
  (drakma-style-stream-values)
  (http-stream-to-vector)
  (vanilla-client-stream type)
  (vanilla-client-connection type)
  (client-stream type)
  (connect-to-tls-server)
  (header-collecting-mixin type)
  (client-http2-connection type)
  (extract-charset-from-content-type))

(cz.zellerin.doc::define-section @api-frames
  "Lowest level interace deals with sending and receiving individual frames. For
each frame type there is a read function (~READ-DATA-FRAME~, ...) and write
function (~WRITE-DATA-FRAME~, ...).

The read functions should not be called from other place than READ-FRAME; this
function reads frame header, identifies the frame in question and calls
appropriate reader code. The exception is continuation frames; they can be
receipt only in specific situations where no other frame type is expected, and
are read in explicitly.

The write function are expected to be called individually and each calls
~WRITE-FRAME-HEADER~ to send common parts; you should not need to call it, but
it is a good one to trace to debug low level problems. Each write function takes
object identifying the http stream or connection that the frame affects,
additional parameters, and optional parameters that usually relate to the known
flags."
  (read-frame)
  (write-frame-header)
  (write-ack-setting-frame)

  . #.(mapcar (lambda (a) (list (intern (format nil "WRITE-~:@(~a~)" a))))
             (map 'list #'frame-type-name *frame-types*)))

(define-section @new-frames-api
  "The individual frame types are defined using ~DEFINE-FRAME-TYPE~ macro; if you
need to define additional frame types, you would also need to increase
~+known-frame-types-count+~ (maybe this should not really be a constant...)."
  (define-frame-type)
  (+known-frame-types-count+ variable))

(cz.zellerin.doc::define-section @callbacks
  "The reader functions for individual frames may call a callback that is supposed
to handle received frame in some way. All callbacks have stream or connection as
the first parameter.

In addition to the behaviour described below, all callback log the behaviour
when relevant stream or connection has logging-object as superclass."
  (apply-data-frame)
  (apply-stream-priority)
  (apply-window-size-increment)
  (peer-resets-stream)
  (set-peer-setting)
  (peer-expects-settings-ack)
  (peer-acks-settings)
  (peer-ends-http-stream)
  (handle-undefined-frame)
  (do-pong)
  (do-goaway))

(cz.zellerin.doc:define-section @server
  ""
  (handler cz.zellerin.doc::macro)
  (constant-handler cz.zellerin.doc::macro)
  (scheduling-handler cz.zellerin.doc::macro)
  (send-text-handler)
  (redirect-handler)
  (define-exact-handler cz.zellerin.doc::macro)
  (define-prefix-handler cz.zellerin.doc::macro)
  (process-server-stream)
  (dispatcher-mixin class)
  (vanilla-server-connection class)
  (vanilla-server-stream class)
  (kill-server restart
               "Kill server and close its underlying network stream. Bound in
CREATE-HTTPS-SERVER."))

(cz.zellerin.doc:define-section @tls-server
  ""
  (create-https-server)
  (wrap-to-tls-and-process-server-stream)
  (*dispatch-fn* variable)
  (threaded-dispatch)
  (create-one-shot-server))
