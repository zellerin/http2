(in-package http2)

(defun create-server (port)
  (let* ((socket (usocket:socket-listen "127.0.0.1" port))
	 (network-stream (usocket:socket-accept socket :element-type 'character))
         (connection (make-instance 'logging-connection :network-stream network-stream)))
    (unwind-protect
         (handler-case
             (loop
               do
                  (read-frame connection)
               until (get-finished connection))
           (end-of-file () nil))

      (force-output (usocket:socket-stream connection))
      (progn
	    (format t "Closing sockets~%")
	    (usocket:socket-close connection)
            (usocket:socket-close socket)))))
