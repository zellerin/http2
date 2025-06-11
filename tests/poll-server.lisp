(in-package http2/tests/server)

(deftest add-and-maybe-pass-data/test ()
  "Test adding new data to chunks."
  (flet ((@ (new-data-size expected-new-size chunk-sizes)
             (let* ((buffer (make-octet-buffer 100))
                    (new-data (make-octet-buffer new-data-size))
                    (outputs nil)
                    (cleaner (lambda (client buffer start to)
                               (declare (ignore client))
                               (push (subseq buffer start to) outputs)
                               (- to start))))
               (fill new-data 42)
               (fill buffer 10 :end 20)
               (multiple-value-bind (new-size processed)
                   (http2/server/poll::add-and-maybe-pass-data nil buffer new-data 0 (length new-data) 10 cleaner)
                 (is (= new-size expected-new-size))
                 (is (equalp (mapcar 'length outputs) chunk-sizes))
                 (is (= processed new-data-size))
                 (values buffer outputs new-size processed)))))
    (@ 10 20 nil)
    (@ 50 60 nil)
    (@ 95 5 '(100))
    (@ 200 0 '(110 100))))

;;;; Sandbox
(defsection @poll-pair ())

(defcfun socket :int "man socket(2)" (domain :int) (type :int) (protocol :int))
(defcfun bind :int "man bind(2)" (sockfd :int) (addr :pointer) (addr-len :int))
(defcfun connect :int "man connect(2)" (sockfd :int) (addr :pointer) (addr-len :int))
(defcfun getsockname :int "man getsockname(2)"
  (sockfd :int) (addr :pointer) (addr-len :pointer))
(defcfun (listen-2 "listen") :int "man listen(2)" (sockfd :int) (backlog :int))

(defun nonneg-or-eagain (res)
  "Helper as first parameter for checked-syscall."
  (or
   (/= res -1)
   (= (http2/server/poll::errno) http2/server/poll::EAGAIN)
   (= (http2/server/poll::errno) http2/server/poll::EINPROGRESS)))

(defun call-with-tcp-pair (fn)
  "Apply FN on two interconnected non-blocking TCP sockets.

Make sure sockets are closed afterwards."
  (with-foreign-objects ((addr '(:struct sockaddr-in)) (len :int))
    (flet ((make-socket ()
             "Make a non-blocking socket bound to localhost:0."
             (let ((socket (checked-syscall #'plusp #'socket 2 1 0)))
               ;; FIXME: leaks socket when something below fails
               (socket-bind (set-nonblock socket)))))
      (let ((a (make-socket))
            (b (make-socket)))
        (unwind-protect
             (unwind-protect
                  (checked-syscall #'zerop #'listen-2 a 1)
               (getsockname a addr len)
               (checked-syscall #'nonneg-or-eagain #'connect b addr size-of-sockaddr-in)
               (let ((srv (checked-syscall #'plusp #'accept a addr len)))
                 (unwind-protect
                   (funcall fn srv b)
                   (close-fd srv)))
               (close-fd a))
          (close-fd b))))))

(defmacro with-tcp-pair ((a b) &body body)
  "Run BODY with A and B bound to connected sockets.

Close thos sockets afterwards."
  `(call-with-tcp-pair (lambda (,a ,b) ,@body)))

(defun print-data (client data)
  (print (list client data))
  (values #'print-data 10))

(defclass certificated-poll-dispatcher (http2/openssl::certificated-dispatcher poll-dispatcher-mixin)
  ())

(defun poll-server-test (&key prepare-fn after-poll-fn (dispatcher 'certificated-poll-dispatcher))
  (let* ((key-file (find-private-key-file "localhost"))
         (dispatcher (make-instance dispatcher
                                    :fdset-size 2

                                    :private-key-file (namestring key-file)
                                    :certificate-file (namestring (find-certificate-file key-file))
                                    :allow-other-keys t)))
    (with-fdset (dispatcher)
      (http2/openssl:with-ssl-context (ctx dispatcher)
        (with-tcp-pair (server-socket client-socket)
          (dolist (socket (list client-socket server-socket))
            (set-nonblock socket))
          (let ((client (make-client client-socket ctx 'client))
                (server (make-client server-socket ctx 'server)))
            (setf (get-clients dispatcher) (list server client)
                  (client-application-data client) client
                  (client-application-data server) server)
            (with-slots (fdset) dispatcher
              (http2/openssl:ssl-accept (client-ssl server))
              (http2/openssl:ssl-connect (client-ssl client))
              (add-socket-to-fdset fdset server-socket server 0)
              (add-socket-to-fdset fdset client-socket client 1)
              (funcall prepare-fn server client)
                   (loop for nread = (poll dispatcher 100)
                    while (plusp nread)
                    do
                       (process-client-sockets nread dispatcher)
                       (funcall after-poll-fn server client)))))))))

(defun ignore-data (client data)
  (declare (ignore client))
  (values #'ignore-data (length data)))

(deftest test-no-certificates ()
  "No certificate on the server. Expected result is an error, as there is no agreed
cipher.

We do not want errors of this kind masked too early."
  (signals http2/openssl:ssl-error-condition
    (poll-server-test
     :dispatcher 'poll-dispatcher-mixin
     :prepare-fn (lambda (client server)
                   (set-next-action client #'ignore-data 1)
                   (set-next-action server #'ignore-data 1)
                   (send-unencrypted-bytes server (make-octet-buffer 1) nil)
                   (send-unencrypted-bytes client (make-octet-buffer 1) nil)
                   (encrypt-and-send client)
                   (encrypt-and-send server))
     :after-poll-fn (constantly nil))))

(defun test-send-in-advance (blob-size)
  (let ((received 0))
    (labels ((receive (client data)
               (incf received (length data))
               (let ((reply (make-octet-buffer 1)))
                 (declare (dynamic-extent reply))
                 (send-unencrypted-bytes client reply nil))
               (encrypt-and-send client)
               (values #'receive 10)))
      (poll-server-test
       :prepare-fn (lambda (client server)
                     (set-next-action client #'receive 10)
                     (set-next-action server #'ignore-data 1)
                     (send-unencrypted-bytes server (make-octet-buffer blob-size) nil)
                     (encrypt-and-send client)
                     (encrypt-and-send server))
       :after-poll-fn (constantly nil)))
    received))

(deftest send-in-advance ()
  (dolist (size '(10 100 200)) ;; note: fails for 2000
    (is (equal size (test-send-in-advance size)))))
