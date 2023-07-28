(load "~/quicklisp/setup")
(ql:quickload 'cz.zellerin.doc)
(ql:quickload 'http2/all)

(in-package http2)
(import '(cz.zellerin.doc:define-section
          cz.zellerin.doc:export-pkg-to-org))

(setf (getf cz.zellerin.doc:*package-sections* 'cz.zellerin.doc::http2)
      '(@client @api-frames @callbacks @new-frames-api))

(define-section @new-frames-api
  "The individual frame types are defined using ~DEFINE-FRAME-TYPE~ macro; if you
need to define additional frame types, you would also need to increase
~+known-frame-types-count+~ (maybe this should not really be a constant...)."
  (define-frame-type)
  (+known-frame-types-count+ variable))

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
