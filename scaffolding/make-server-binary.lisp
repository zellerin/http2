;;;; Copyright 2022 by Tomáš Zellerin

;;; this file was tested to run with sbcl, clisp. Runs with ecl as well, but
;;; resulting binary fails.

(load "~/quicklisp/setup")
(asdf::load-asd (truename "./http2.asd"))
(ql:quickload "http2/server")

;; store executable locally, and possibly compressed.
#-ecl
(defmethod asdf:perform ((o asdf:program-op) (c asdf:system))
  (uiop:dump-image "http2-server" :executable t
                   . #+sb-core-compression (:compression t)
                     #-sb-core-compression nil))

(asdf:oos :program-op "http2/server/example")
