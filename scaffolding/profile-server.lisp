#!/usr/bin/env sbcl --script

;;;; Run a web server on port 8088 that after connection is closed writes
;;;; allocation profile of the run.
;;;;
;;;; That is a http (not https) server.
;;;;
;;;; Use something like
;;;;    h2load http://localhost:8088/ -n 10000
;;;;

(load "~/.sbclrc") ; to setup asdf locations and possibly quicklisp

;;;; TODO: fix warnings in load
(asdf:load-system "http2/server")

(in-package http2/server)
(require 'sb-sprof)

(defclass measured-dispatcher (poll-dispatcher)
  ())

(defmethod do-new-connection (socket (dispatcher measured-dispatcher))
  (sb-sprof:reset)
  (sb-sprof:with-profiling (:mode :alloc :threads (list sb-thread:*current-thread*))
    (call-next-method))
  (sb-sprof:report))


(sb-sprof:with-profiling (:mode :time :threads (list sb-thread:*current-thread*))
    (handler-case
        (start 8088 :dispatcher 'measured-dispatcher )
      (sb-sys:interactive-interrupt ())))
(with-open-file (out "/tmp/foo.out" :direction :output :if-exists :supersede) (sb-sprof:report :max 20  :stream out))
