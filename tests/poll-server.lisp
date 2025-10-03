(in-package http2/tests/server)

(deftest add-and-maybe-pass-data/test ()
  "Test adding new data to the chunks."
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

(defun call-with-clients-pair (fn &key (dispatcher (make-instance 'poll-dispatcher :fdset-size 3))
                                    (server-context 'poll-dispatcher)
                                    (client-context t))
  "Call FN with two parameters, client and server, that are TLS endpoint objects
with interconnected file descriptor.

DISPATCHER describes how the FDSET is created and handled (see WITH-FDSET).

SERVER-CONTEXT and CLIENT-CONTEXT define how the TLS context are created, see
MAKE-HTTP2-TLS-CONTEXT."
  (let ((http2/server/poll::*encrypted-buf-size* 3000))
    (when (symbolp server-context) (setf server-context (make-instance server-context)))
    (when (and (not (eql client-context t)) (symbolp client-context)) (setf client-context (make-instance client-context)))
    (with-fdset (dispatcher)
      (with-tcp-pair (s c)
        (http2/server/poll::with-ssl-context (server-ctx server-context)
          (http2/server/poll::with-ssl-context (client-ctx client-context)
            (set-nonblock s)
            (set-nonblock c)
            (let ((server (make-tls-endpoint s server-ctx 'server (add-new-fdset-item s dispatcher)))
                  (client (make-tls-endpoint c client-ctx 'client (add-new-fdset-item c dispatcher))))
              ;; Initialize client: connect it and send client hello
              (setf (get-clients dispatcher) (list server client))
              (http2/server/poll::remove-state client 'http2/server/poll::ssl-init-needed)
              (http2/server/poll::ssl-connect (http2/server/poll:client-ssl client))
              (http2/server/poll::add-state client 'http2/server/poll::can-read-bio)
              (http2/server/poll::do-available-actions client)
              (encrypt-and-send client)
              (sleep 0.1) ; Naggle
              (http2/server/poll::add-state server 'http2/server/poll::can-read-port)
              (http2/server/poll::do-available-actions server)
              (encrypt-and-send server) ; send encrypted data (80+26 octets)
              (sleep 0.1) ; Naggle
              (http2/server/poll::add-state client 'http2/server/poll::can-read-port)
              (http2/server/poll::do-available-actions client)
              (http2/server/poll::remove-state client 'http2/server/poll::can-read-bio)
              (http2/server/poll::do-available-actions client)
;              (send-unencrypted-bytes client (make-initialized-octet-buffer #(1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4)) nil)
;              (send-unencrypted-bytes client (make-initialized-octet-buffer #(1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4)) nil)
;              (send-unencrypted-bytes client (make-initialized-octet-buffer #(1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4)) nil)
              (encrypt-and-send client) ; send encrypted data (80+26 octets)
              (http2/server/poll::do-available-actions client)

              (sleep 0.1) ; Naggle
              (http2/server/poll::add-state server 'http2/server/poll::can-read-port)
              (http2/server/poll::do-available-actions server)
              (encrypt-and-send server) ; send encrypted data (80+26 octets)

              (funcall fn server client))))))))

(deftest write-read-peer/test-no-tls ()
  "Write fixed data to server and see it on the client."
#+nil  (call-with-clients-pair
   (lambda (server client)
     (http2/server/poll::send-to-peer server (make-initialized-octet-buffer #(1 2 3 4)) 0 4)
     (http2/server/poll::send-to-peer client (make-initialized-octet-buffer #(5 6 7 8)) 0 4)
     (let ((res  (make-octet-buffer 100)))
       (is (= 4 (http2/server/poll::read-from-peer client res 100)))
       (is (equalp (subseq res 0 4) #(1 2 3 4))))
     (let ((res  (make-octet-buffer 100)))
       (is (= 4 (http2/server/poll::read-from-peer server res 100)))
       (is (equalp (subseq res 0 4) #(5 6 7 8)))))))

(deftest write-read-peer-tls/test ()
  "Initialize and make through the communication simulating TLS connection and a
single data send.

Check that the other endpoint got the message.

This does not use POLL yet; instead, the appropriate state is added manually."
#+nil
  (call-with-clients-pair
   (lambda (server client)
     (flet ((@ (endpoint)
                (http2/server/poll::add-state endpoint 'http2/server/poll::can-read-port)
              (http2/server/poll::do-available-actions endpoint)))
       (send-unencrypted-bytes client (make-initialized-octet-buffer #(1 2 3 4)) nil)
       (encrypt-and-send client)        ; send encrypted data (80+26 octets)
       (@ server)
       (@ client)
       (is (equalp
            #(1 2 3 4)
            (http2/server/poll::get-received (signals http2/server/poll::not-enough-data (@ server)))))))))

(defun print-data (client data)
  (print (list client data))
  (values #'print-data 10))

(defclass certificated-poll-dispatcher (http2/server::certificate-h2-dispatcher poll-dispatcher-mixin)
  ())

(defun poll-server-test (&key prepare-fn after-poll-fn (dispatcher (make-instance 'poll-dispatcher)))
  "Make a server-client TCP pair (nonblocking), let them establish the TLS connection (accept-connect),
define CLIENT and SERVER as poll clients, call PREPARE-FN with SERVER and client
as the parameters, and then read and write messages among them and call the
AFTER-POLL-FN on them after data exchange."
  (declare (function prepare-fn after-poll-fn))
  (call-with-clients-pair
   (lambda (server client)
     (funcall prepare-fn server client)
     (loop for nread = (poll dispatcher 0)
           while (plusp nread)
           do
              (process-client-sockets nread dispatcher)
              (funcall after-poll-fn server client)))
   :dispatcher dispatcher))

(defun ignore-data (client data)
  (declare (ignore client))
  (values #'ignore-data (length data)))

(deftest write-read-peer-tls/test-no-certificates ()
  (let ((err
          (signals http2/openssl:ssl-error-condition
            (call-with-clients-pair
             (constantly nil)
             :server-context ""))))
    (is (= (http2/server/poll::get-code err) #xa0000c1)))) ; no shared cipher

(defun test-send-in-advance (blob-size)
  "Send BLOB-SIZE octets from one TLS endpoint to another before the TLS connection
is set up. This should be queued and used at the very start of the communication.

Return number of received octets (that should be same as number of octets sent)"
  #+nil
  (let ((received 0))
    (poll-server-test
     :prepare-fn (lambda (client server)
                   (labels ((receive (c data)
                              (incf received (length data))
                              (let ((reply (make-octet-buffer 1)))
                                (declare (dynamic-extent reply))
                                (send-unencrypted-bytes client reply nil))
                              (encrypt-and-send client)
                              (values #'receive 10)))
                     (set-next-action client #'receive 10)
                     (set-next-action server #'ignore-data 1)
                     (send-unencrypted-bytes server (make-octet-buffer blob-size) nil)
                     (encrypt-and-send client)
                     (encrypt-and-send server)))
     :after-poll-fn (constantly nil))
    received))

(deftest send-in-advance ()

  #+nil
  (dolist (size '(10 100 200 1000)) ;; note: fails for 2000
    (is (equal size (test-send-in-advance size)))))

(defsection @ssltests (:title "SSL interface tests")
  (tls-low-level-demo function)
  (client-connect function))

(flet ((pass-data (from to)
         (when (plusp  (http2/server/poll::client-write-buf-size from))
           (http2/server/poll::write-octets-to-decrypt to (http2/server/poll::client-write-buf from)
                                                       0 (http2/server/poll::client-write-buf-size from))
           (setf (http2/server/poll::client-write-buf-size from) 0)))
       (move-encrypted (endpoint)
         (http2/server/poll::move-encrypted-bytes endpoint)))


  (defun client-connect (client server)
    (http2/server/poll::ssl-connect (http2/server/poll:client-ssl client))
    (move-encrypted client)
    client)

  (defun server-accept (client server)
    (pass-data client server)
    (http2/server/poll::handle-ssl-errors server (http2/server/poll::ssl-accept (client-ssl server)))
    (move-encrypted server)
    server)

  (defun --> (data size)
    (lambda (client server)
      (pass-data server client)
      (http2/server/poll::encrypt-some client data 0 size)
      (move-encrypted client)
      client))

  (defun move-all (client server)
    (pass-data server client)
    (pass-data client server))

  (defun <-- (data size)
    (lambda (client server)
      (pass-data client server)
      (http2/server/poll::encrypt-some server data 0 size)
      (move-encrypted server)
      server)))

(defun tls-low-level-demo (actions &optional (final #'describe))
  "Test low-level communication between TLS endpoints.

ACTIONS is a list of functions to call on TLS endpoints. Some functions designed
to be included there are (--> ...), (<-- ...), client-connect and server-accept,
but any function of two arguments, CLIENT and SERVER, can be used.

FINAL is called with the result of last action as the only parameter.

Example:

```cl-transcript
(tls-low-level-demo `(client-connect
                      server-accept
                     ,(--> (make-initialized-octet-buffer '(1 2 3 4 5 6 7 8 9 42)) 10)
                     ,(<-- (make-octet-buffer 10) 10)))
.. A TLS endpoint for NIL
..    It is associated with file descriptor -1.
..    It has position -1 in the FDSET.
..    It expects 24 octets that would be processed by #<FUNCTION HTTP2/CORE:PARSE-CLIENT-PREFACE> with application
..    Buffers:
..       - Encrypt buffer is empty
..       - Write buffer has 542 octets #(23 3 3 0 250)
..            ((:CONTENT-TYPE :APPLICATION :VERSION (3 . 3) :LENGTH 255)
..             (:CONTENT-TYPE :APPLICATION :VERSION (3 . 3) :LENGTH 255)
..             (:CONTENT-TYPE :APPLICATION :VERSION (3 . 3) :LENGTH 32))
..       - TLS is initialized
..       - TLS peek: #(1 2 3 4 5 6 7 8 9 42)
..
..    State: CAN-WRITE-SSL CAN-WRITE HAS-DATA-TO-WRITE BIO-NEEDS-READ SSL-INIT-NEEDED
..
```"
  (let ((http2/server/poll::*encrypted-buf-size* 3000)
        (http2/server/poll::*describe-object-buffer-limit* 5)) ;; all TLS messages need to fit the write buffer
    (http2/server/poll::with-ssl-context (server-ctx (make-instance 'poll-dispatcher))
      (http2/server/poll::with-ssl-context (client-ctx t)
        (with-tls-endpoint (server server-ctx)
          (with-tls-endpoint (client client-ctx)
            (let (last)
              (dolist (action actions)
                (setf last (funcall action client server)))
              (funcall final last))))))))
