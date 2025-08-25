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

(defclass client-context ()
  ())

(defmethod make-http2-tls-context ((context client-context))
  (http2/openssl::ssl-ctx-new (http2/openssl::tls-method)))

(defun call-with-clients-pair (fn)
  (with-tcp-pair (s c)
    (http2/server/poll::with-ssl-context (context (make-instance 'poll-dispatcher-mixin))
      (http2/server/poll::with-ssl-context (client-context (make-instance 'client-context))
        (funcall fn (make-client s context nil) (make-client c client-context nil))))))

(deftest write-read-peer/test ()
  "Write fixed data to server and see it on the client"
  (call-with-clients-pair
   (lambda (server client)
     (http2/server/poll::send-to-peer server (make-initialized-octet-buffer #(1 2 3 4)) 0 4)
     (http2/server/poll::send-to-peer client (make-initialized-octet-buffer #(5 6 7 8)) 0 4)
     (let ((res  (make-octet-buffer 100)))
       (is (= 4 (http2/server/poll::read-from-peer client res 100)))
       (is (equalp (subseq res 0 4) #(1 2 3 4))))
     (let ((res  (make-octet-buffer 100)))
       (is (= 4 (http2/server/poll::read-from-peer server res 100)))
       (is (equalp (subseq res 0 4) #(5 6 7 8)))))))

(deftest write-tls-read-peer/test ()
  (call-with-clients-pair
   (lambda (server client)

     (http2/server/poll::ssl-connect (http2/server/poll:client-ssl client))
     (http2/server/poll::remove-state client 'http2/server/poll::ssl-init-needed)
     (http2/server/poll::add-state client 'http2/server/poll::can-read-bio)
     (http2/server/poll::do-available-actions client)

     (let ((res (make-octet-buffer 100)))
       (set-nonblock (http2/server/poll::client-fd server))
       (= 4 (http2/server/poll::read-from-peer server res 100))
       (equalp (subseq res 0 4) #(1 2 3 4))
       res))))

(defun print-data (client data)
  (print (list client data))
  (values #'print-data 10))

(defclass certificated-poll-dispatcher (http2/openssl::certificated-dispatcher poll-dispatcher-mixin)
  ())

(defun poll-server-test (&key prepare-fn after-poll-fn (dispatcher 'certificated-poll-dispatcher))
  "Make a server-client TCP pair (nonblocking), let them establish the TLS connection (accept-connect),
define CLIENT and SERVER as poll clients, call PREPARE-FN with SERVER and client
as the parameters, and then read and write messages among them and call the
AFTER-POLL-FN on them after data exchange."
  (declare (function prepare-fn after-poll-fn))
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
                   (http2/server/poll::ssl-connect (client-ssl client))
                   (send-unencrypted-bytes client (make-octet-buffer 1) nil)
                   (encrypt-and-send client)                   )
     :after-poll-fn (lambda (client server)
                      (describe client)
                      (describe server)
                      (http2/server/poll::do-available-actions client)
                      (http2/server/poll::do-available-actions server)))))

(defun test-send-in-advance (blob-size)
  "Send BLOB-SIZE octets from one TLS endpoint to another before the TLS connection
is set up. This should be queued and used at the very start of the communication.

Return number of received octets (that should be same as number of octets sent)"
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
