(in-package #:http2/server/poll)


(defsection  @async-server (:title "Polling server overview")
  #+nil  (client type)
  (@app-interface mgl-pax:section)
  (@request-handling mgl-pax:section))

(defsection @app-interface (:title "Interface to the polling server")
  "POLL-DISPATCHER-MIXIN tracks a pool of clients that are connected to a
socket. Each client has a user registered callback function and required number
of octets to have available to call the callback. Callbacks call
SEND-UNENCRYPTED-BYTES to encrypt and send more data.

The central function is SERVE-TLS that orchestrates reading, decrypting,
encrypting and writing of data for all the sockets."
  (get-clients function)
                                        ; (client-application-data function)
  (client-ssl function)
  (poll-dispatcher-mixin class)
  (process-client-sockets function)
  (add-socket-to-fdset function)
  (send-unencrypted-bytes function)
  (encrypt-and-send function)
  (set-next-action function)
;  (get-fdset-size (method  (poll-dispatcher-mixin)))
  (get-poll-timeout (method (poll-dispatcher-mixin)))
  (get-no-client-poll-timeout (method (poll-dispatcher-mixin)))
  #+nil ((ssl-read function)
         (send-unencrypted-bytes function))
  ;; Timeouts
  (*no-client-poll-timeout* variable)
  (*poll-timeout* variable)
  (poll-timeout condition)
  (compute-poll-timeout-value function))

(defsection @tls-endpoint (:title "TLS endpoint")
  ""
  (tls-endpoint type)
  (make-tls-endpoint function)
  (with-tls-endpoint macro))

(defsection @request-handling
    (:title "Client actions loop (implementation)"
     :export nil)
  "Each TLS-ENDPOINT has a STATE that encapsulates what actions are effectively
possible.

SELECT-NEXT-ACTION selects appropriate action. When no new action is available,
next client is handled and eventually POLL called when all clients were served.

The actions are in general indicated by arrows in the diagram:

![](poll-server.svg)"

  (@tls-endpoint section)
  (do-available-actions function)

  (@application-loop section)
  (@port-to-tls section)
  (@tls-to-port section))

(defsection @application-loop (:title "Application loop")
  ; Interface (send-unencrypted-bytes function)
  (on-complete-ssl-data function)
  (run-user-callback function)
  (encrypt-data function)
  (encrypt-some function)
  (encrypt-and-send function))

(defsection @port-to-tls (:title "Port to TLS")
  (process-data-on-socket function)
  (read-from-peer function)
  (decrypt-socket-octets function)
  (write-octets-to-decrypt function))

(defsection @tls-to-port (:title "TLS to port")
  (move-encrypted-bytes function)
  (write-data-to-socket function))

(deftype buffer-size () `(integer 0 ,array-dimension-limit))
(declaim (buffer-size *default-buffer-size* *encrypt-buf-size*))

(defvar *encrypt-buf-size* 256
  "Initial size of the vector holding data to encrypt.")

(defvar *encrypted-buf-size* 1500
  "Initial size of the vector holding encrypted data")

(defvar *default-buffer-size* 1500) ; close to socket size


(defsection @poll-tls-states (:title "TLS endpoint states")
  "The actions available for a specific endpoint are kept in STATE.

Each state bit corresponds to one function that can be called."
  "CAN-READ-PORT is set when there are data available on the input port. This can
be set by HANDLE-CLIENT-IO after poll, and is cleared by READ-FROM-PEER when there are
no longer data available. It allows PROCESS-DATA-ON-SOCKET to be called."
  "CAN-READ-SSL is set when there are data available on SSL to read by the
application. It is set by PROCESS-DATA-ON-SOCKET, as it indicates that some data
to decrypt were written, and is cleared by SSL-READ. It triggers
ON-COMPLETE-SSL-DATA or RUN-USER-CALLBACK."
  "CAN-WRITE-SSL is set when data can be written to SSL. It is set by
PROCESS-DATA-ON-SOCKET and cleared by ENCRYPT-SOME. Triggers ENCRYPT-DATA."
  "CAN-READ-BIO is set when there are probably some data to read from the BIO. It
is set by ENCRYPT-SOME and PROCESS-DATA-ON-SOCKET and MAYBE-INIT-SSL. It is
cleared by READ-ENCRYPTED-FROM-OPENSSL.  It triggets MOVE-ENCRYPTED-BYTES."
  "CAN-WRITE is set when writing to the output socket is possible (which usually
is). It is set by HANDLE-CLIENT-IO and . It is cleared by SEND-TO-PEER and
WRITE-DATA-TO-SOCKET. It triggers WRITE-DATA-TO-SOCKET."
  "HAS-DATA-TO-WRITE is set when the write buffer for output socket is
non-empty (or, not implemented, has enough data to make sending economical). It
is set by READ-ENCRYPTED-FROM-OPENSSL and MOVE-ENCRYPTED-BYTES. It is cleared by
WRITE-DATA-TO-SOCKET and triggers MOVE-ENCRYPTED-BYTES."
  "NEG-BIO-NEEDS-READ is set by PROCESS-DATA-ON-SOCKET and triggers
MAYBE-INIT-SSL. It is cleared by an error condition in HANDLE-SSL-ERRORS."
  "SSL-INIT-NEEDED is maybe not needed?"
  (state type)

  (select-next-action function)
  (states-to-string function))

;;;; Async TLS endpoint state
(eval-when (:load-toplevel :compile-toplevel)
  (defparameter *states*
    '(CAN-READ-PORT                     ; ①
      CAN-READ-SSL                      ; ③
      CAN-WRITE-SSL                     ; ④
      CAN-READ-BIO                      ; ⑤
      CAN-WRITE                         ; ⑥
      HAS-DATA-TO-WRITE                 ; ⓤ
      NEG-BIO-NEEDS-READ                ; B
      SSL-INIT-NEEDED                   ; S
      )
    "List of state bits that can a TLS endpoint have."))

(defun states-to-string (state)
  "Short string describing the state using codes on the diagram."
  (with-output-to-string (*standard-output*)
    (loop ;for state in *states*
          for state-idx from 0
          for label across "①③④⑤⑥ⓤⒺBSO"
          do (princ
              (if (plusp (ldb (byte 1 state-idx) state)) label #\Space)))))

(deftype state ()
  "Description of actions available to the endpoint."
  `(unsigned-byte ,(length *states*)))

(defmacro state-idx (state)
  `(let ((idx (position ,state ',*states*)))
     (or idx (error "No state ~a" ,state))))

(defun if-state* (client state-idx)
  (plusp (ldb (byte 1 state-idx)
              (client-state client))))

(declaim (inline if-state add-state remove-state if-state* test-state*))

(defun if-state (client state)
  (if-state* client (state-idx state)))

(defun set-state* (client idx value)
  (declare (bit value)
           (fixnum  idx))
  (setf (ldb (byte 1 idx)
             (client-state client))
        value))

(defun add-state (client state)
  (set-state* client (state-idx state) 1))

(defun remove-state (client state)
  (set-state* client (state-idx state) 0))

(defparameter *initial-state*
  (loop with state = 0
        for item in
        '(CAN-WRITE CAN-WRITE-SSL ssl-init-needed)
        do (setf (ldb (byte 1 (state-idx item)) state) 1)
        finally (return state)))


(deftype app-callback ()
  `(and compiled-function
        (function (t octet-vector) (values compiled-function buffer-size))))

(defstruct (tls-endpoint (:constructor make-client%)
                         (:include tls-endpoint-core)
                         (:conc-name "CLIENT-")
                         (:print-object
                          (lambda (object out)
                            (format out "tls endpoint for ~a" (client-application-data object)))))
  "Data of one TLS endpoint that is connected to a socket that is part of a FDSET. This includes:

- File descriptor of underlying socket (FD).
- Opaque pointer to the openssl handle (SSL). See SSL-READ and ENCRYPT-SOME.
- Input and output BIO for exchanging data with OPENSSL (WBIO, RBIO).
- Buffer of plain text octets to encrypt and send (ENCRYPT-BUF). This buffer reduces TLS records overhead.
- Buffer of encrypted octets to send to the file descriptor (WRITE-BUF). This buffer reduces network headers overhead and removes delays due to Nagle algorithm.
- Callback function when read data are available (IO-ON-READ) and Number of octets required by it (OCTETS-NEEDED). Negative values have special handling. See RUN-USER-CALLBACK for details.
- Client state from the low-level data flow point of view (STATE).
- Application data (slot to be used by the application). This is usually some application structure, but can be also a symbol in tests. Another input to RUN-USER-CALLBACK."
  (fd -1 :type fixnum)
  (write-buf (make-array *encrypted-buf-size* :element-type '(unsigned-byte 8))
   :type (and (simple-array (unsigned-byte 8))))
  (write-buf-size 0 :type fixnum)
  (encrypt-buf (make-array *encrypt-buf-size* :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8))
   )
  (io-on-read (constantly nil) :type app-callback)
  (fdset-idx 0 :type fixnum :read-only nil) ; could be RO, but...
  (octets-needed 0 :type fixnum)
  (encrypt-buf-size 0 :type fixnum)
  (start-time (get-internal-real-time) :type fixnum)
  (state *initial-state* :type state)
  ;; set of CAN-READ-PORT, CAN-READ-SSL, CAN-WRITE-SSL,
  ;; CAN-READ-BIO, HAS-DATA-TO-WRITE, CAN-WRITE
  ;; NEG-BIO-NEEDS-READ SSL-INIT-NEEDED
  (application-data))

(defvar *tls-content-types*
  '(20 :change-cipher-spec 21 :alert 22 :handshake 23 :application 24 :heartbeat))

(defvar *tls-message-types*
  '(0 :hello-request 1 :client-hello 2 :server-hello 4 :new-session-ticket))

(defun parse-tls-record (octets start end)
  "Parse a TLS record"
  (handler-case
      (loop for size = (+ 5 (aref/wide octets (+ start 3) 2))
          while (< start end)
          collect
          (list* :content-type (getf *tls-content-types* (aref octets start) (aref octets start))
                 :version (cons (aref octets (1+ start)) (aref octets (+ start 2)))
                 :length size
                 (when (= (aref octets start) 22)
                   `(:type ,(getf *tls-message-types* (aref octets (+ start 5)) (aref octets (+ start 5))))))
            do (incf start size)
            finally (unless (= start end) (warn "PARSE-TLS: partial message ~d/~d"  start end)))))

(defvar *describe-object-buffer-limit* nil)

(defmethod describe-object ((object tls-endpoint) stream)
  (let ((*print-length* (or *print-length* 30)))
    (format stream "~&A TLS endpoint for ~a
   It is associated with file descriptor ~d.
   It has position ~D in the FDSET.
   It expects ~d octets that would be processed by ~a with application
   Buffers:~@<
      - Encrypt buffer ~:[has ~d octets ~s~;~2*is empty~]
      - Write buffer ~:[has ~d octets ~s~_~s~;~3*is empty~]
      - TLS is ~:[NOT ~;~]initialized
      - TLS peek: ~s
   ~:>
   State:"
            (client-application-data object)
            (client-fd object)
            (client-fdset-idx object) (client-octets-needed object) (client-io-on-read object)
            (zerop (client-encrypt-buf-size object))
            (client-encrypt-buf-size object) (subseq (client-encrypt-buf object) 0 (client-encrypt-buf-size object))
            (zerop (client-write-buf-size object))
            (client-write-buf-size object) (subseq (client-write-buf object) 0 *describe-object-buffer-limit*)
            (parse-tls-record (client-write-buf object) 0  (client-write-buf-size object))
            (plusp (ssl-is-init-finished (client-ssl object)))
            (when (plusp (ssl-is-init-finished (client-ssl object))) (ssl-peek object 100))))
  (loop
    with state = (client-state object)
    for state-name in *states*
    for state-idx from 0
    if (plusp (ldb (byte 1 state-idx) state))
      do (format stream " ~a" state-name))
  (terpri stream))

(defmacro define-reader (name source args &body body &aux declaration)
  (when (eq (caar body) 'declare)
    (setq declaration (pop body)))
  (destructuring-bind (client vector size) args
    (declare (ignore client))
    `(progn
       (defun ,name ,args
         ,(format nil "Move up to ~a octets from ~a to the ~a.~2%Return 0 when no data are
available. Raise an error on error." size source vector)
         ,declaration
         ,@body)
       (setf (get ',name 'source) ',source))))

(defmacro define-writer (name destination args &body body &aux declaration)
  (destructuring-bind (client vector from to) args
    (declare (ignore client from to))
    (when (eq (caar body) 'declare)
      (setq declaration (pop body)))
    `(progn
       (defun ,name ,args
         ,(format nil "Move octets from ~a to the ~a.~2%Return 0 when no data are
available. Raise an error on error." vector destination)
         ,declaration
         (progn ,@body))
       (setf (get ',name 'destination) ',destination))))

;;;; Input port
(defsection @socket-setup (:title "Socket setup")
  (checked-syscall function)
  (set-nonblock function))

(defun setup-port (socket nagle)
  "Set the TCP socket: nonblock and possibly nagle."
  (set-nonblock socket)
  (unless nagle
    (set-nodelay socket)))

(define-reader read-from-peer peer-in (client vec vec-size)
  (let ((read (read-buffer (client-fd client) vec vec-size)))
    (unless (= read vec-size)
      (remove-state client 'CAN-READ-PORT))
    read))

;;;; Read BIO (rbio)

;;; This name is somewhat confusing - it is BIO for SSL reads, so it actually
;;; gets written to.

(define-writer write-octets-to-decrypt openssl-to-decrypt (client vector from to)
  (with-pointer-to-vector-data (buffer vector)
    (let ((written (bio-write (client-rbio client)
                              (inc-pointer buffer from)
                              (- to from))))
      (unless (plusp written) (error "Bio-write failed"))
      written)))

(defun decrypt-socket-octets (client vector from to)
  "Send data in the VECTOR between FROM and TO to the ② openssl for decryption ."
  (push-bytes client
              (constantly nil)
              #'write-octets-to-decrypt vector from to))

(defun process-data-on-socket (client)
  "Read data from client socket ① and pass them to the tls buffer ② to decrypt."
  (pull-once-push-bytes client #'read-from-peer #'decrypt-socket-octets)
  (add-state client 'CAN-READ-SSL)
  (add-state client 'can-write-ssl)
  (unless (if-state client 'neg-bio-needs-read)
    (add-state client 'neg-BIO-NEEDS-READ)
    (add-state client 'CAN-READ-BIO)))

;;;; Read SSL
(defun ssl-read (client vec size)
   "Move up to SIZE octets from the decrypted SSL ③ to the VEC.

Return 0 when no data are available. Possibly remove CAN-READ-SSL and/or
NEG-BIO-NEEDS-READ flags."
   (let ((res
          (with-pointer-to-vector-data (buffer vec)
            (ssl-read% (client-ssl client) buffer size))))
     (handle-ssl-errors client res)
     (unless (= res size) (remove-state client 'can-read-ssl))
     (max 0 res)))

(defun ssl-peek (client max-size)
   "Move up to SIZE octets from the decrypted SSL ③ to the VEC.

Return 0 when no data are available."
  (unless (null-pointer-p (client-ssl client))
    (let* ((vec (make-octet-buffer max-size))
           (res
             (with-pointer-to-vector-data (buffer vec)
               (http2/openssl::ssl-peek% (client-ssl client) buffer max-size))))
      (handle-ssl-errors client res)
      (values (subseq vec 0 (max 0 res)) res))))

;;;; Encrypt queue
(defun add-and-maybe-pass-data (client buffer new-data from to old-size cleaner)
  "Add new data to buffer if they fit.

If they do not, send it in several parts:
- send the allocated buffer filled up to the max with new data,
- send the new data as bulk if the rest is big enough

Return new buffer actual size and number of new-data octets processed.

The NEW-DATA vector is not stored (can be dynamic-extent)."
  (declare ((function (t octet-vector fixnum fixnum) fixnum) cleaner))
  (replace buffer new-data :start1 old-size :start2 from)
  (let* ((max-size (length buffer))
         (gap (- max-size old-size))
         (new-data-size (- to from)))
    (when (>= gap new-data-size)
      (return-from add-and-maybe-pass-data (values (+ old-size new-data-size) new-data-size)))
    ;; The new data fill the buffer and some more
    (let ((sent (funcall cleaner client buffer 0 max-size)))
      (unless (= sent max-size)
        (error "(unimplemented) Could not process data, sent ~a from max ~a, writer ~a" sent max-size cleaner)
        #+nil        (replace buffer buffer :start2 sent)
        #+nil        (return-from add-and-maybe-pass-data (values (- max-size sent) gap))))
    ;; now the buffer is emptied and we processed some data
    (incf from gap)
    (decf new-data-size gap)
    (when (< new-data-size max-size) ; actually, it might be a bit less strict check
      (replace buffer new-data :start2 from)
      (return-from add-and-maybe-pass-data (values new-data-size (+ gap new-data-size))))
    ;; try to send new data as a bulk
    (let ((sent (funcall cleaner client new-data from to)))
      (unless (= sent new-data-size)
        (error "Could not process data. Implement me.")))
    (values 0 (+ gap new-data-size))))

(defun send-unencrypted-bytes (client new-data comment)
  "Add new data to be encrypted and sent to client.

Data are buffered in the ENCRYPT-BUF of the client and when there is enough of
them they are encrypted.

NEW-DATA are completely used up (can be dynamic-extent)."
  (declare (ignore comment))
  (multiple-value-bind (new-size processed)
      (add-and-maybe-pass-data client
                               (client-encrypt-buf client)
                               new-data 0 (length new-data)
                               (client-encrypt-buf-size client)
                               #'encrypt-some)
    (setf (client-encrypt-buf-size client) new-size)
    (assert (= processed (length new-data)))))

;;;; Write to SSL
(define-writer encrypt-some output-ssl (client vector from to)
  (handler-case
      (let ((res
              (encrypt-some* client vector from to)))
        (when (plusp res)
          (add-state client 'can-read-bio))
        res)
    (ssl-blocked ()
      (remove-state client 'can-write-ssl))))

(defun encrypt-data (client)
  "Encrypt data in client's ENCRYPT-BUF.

Do nothing if there is no data to encrypt or SSL not yet initialized (and return zero).

Otherwise, use a temporary vector to write data "
  (push-bytes client
              (lambda (client written)
                (declare (ignore written))
                (setf (client-encrypt-buf-size client) 0))
              (lambda (client vector from to) (encrypt-some client vector from to))
              (client-encrypt-buf client) 0 (client-encrypt-buf-size client)))

;;;; Write BIO
(define-reader read-encrypted-from-openssl bio-out (client vec size)
  (declare ((simple-array (unsigned-byte 8)) vec)
           (fixnum size))
  (with-pointer-to-vector-data (buffer vec)
    (let ((res (bio-read% (client-wbio client) buffer size)))
      (cond ((plusp res)
             (add-state client 'has-data-to-write)
             res)
            ((zerop (bio-should-retry  (client-wbio client)))
             (error "Failed to read from bio, and cant retry"))
            (t
             (remove-state client 'can-read-bio)
             0)))))

(defun move-encrypted-bytes (client)
  "Move data encrypted by OpenSSL to the socket write queue Ⓔ.

This should be called in a way friendly to Nagle algorithm. My understaning is
this is either when we pipeline a lot of data, or when we send out somethinf
that expects a response."
  (pull-push-bytes client #'read-encrypted-from-openssl #'queue-encrypted-bytes)
  (add-state client 'has-data-to-write))

;;;; TCP write port
(define-writer send-to-peer peer-out (client vector from to)
  (multiple-value-bind (res err) (write-buffer* (client-fd client) vector from to)
    (cond ((and (= res -1)
                (= err eagain))
           (remove-state client 'can-write)
           0)
          ((= res -1)
           ;; e.g., broken pipe
           (error 'syscall-error :errno (errno) :medium client ))
          ((plusp res) res)
          (t (error "This cant happen (#1)")))))

(defun write-data-to-socket (client)
  "Write buffered encrypted data Ⓔ to the client socket ⑥. Update the write buffer to
keep what did not fit."
  (let ((concated (client-write-buf client))) ;;DEBUG
    (let ((written
            (push-bytes client (constantly nil)
                         #'send-to-peer
                         concated 0 (client-write-buf-size client))))
      (cond ((= written (client-write-buf-size client))
             (remove-state client 'has-data-to-write)
             (setf (client-write-buf-size client) 0))
            ((plusp written)
             (remove-state client 'can-write)
             (warn "This should be rare: too much data to encrypt")
             (replace (client-write-buf client) (client-write-buf client)
                      :start2 written :end2 (client-write-buf-size client))
             (decf (client-write-buf-size client) written))
            (t (error "Write failed")))
      written)))

(defun encrypt-and-send (client)
  (unless (plusp (client-fd client)) (error 'end-of-file :stream client))
  (when (plusp (client-encrypt-buf-size client)) (encrypt-data client))
  (move-encrypted-bytes client)
  (write-data-to-socket client))

;;;; Action selector
(defun run-user-callback (client vec)
  "Run user defined callback.

The callback is called with two parameters, client application data (opaque) and vector of OCTETS-NEEDED octets. It should return two values, new callback and new expected size."
  (multiple-value-call #'set-next-action client
    (funcall (client-io-on-read client) (client-application-data client) vec)))

(defun select-next-action (client)
  "One of possible next actions consistent with the state of the client, or
nil if no action is available.

This is factored out so that it can be traced. There is a
TLS-SERVER/MEASURE::ACTIONS clip on this function."
  (cond
    ((if-state client 'can-read-port) #'process-data-on-socket)
    ((and (if-state client 'ssl-init-needed)
          (if-state client 'neg-bio-needs-read))
     #'maybe-init-ssl)
    ((if-state client 'can-read-ssl)
     (if (plusp (client-octets-needed client))
         #'on-complete-ssl-data
         ;; FIXME: this is clearly wrong. The original idea was to run what we
         ;; have on negative octets needed.
         #'run-user-callback))
    ((and (plusp (client-encrypt-buf-size client))
          (if-state client 'can-write-ssl))
     #'encrypt-data)
    ((if-state client 'can-read-bio) #'move-encrypted-bytes)
    ((and (if-state client 'has-data-to-write)
          (if-state client 'can-write))
     #'write-data-to-socket)
    (t nil)))

; (trace select-next-action :print (states-to-string (client-state (sb-debug:arg 0))) :condition-after (sb-debug:arg 0))

(defun do-available-actions (client)
  "Run available actions as selected by SELECT-NEXT-ACTION till there is none."
  (loop
    for action = (select-next-action client)
    while action do (funcall action client)))


;; Moving data around
(deftype reader () '(function (t t fixnum) fixnum))
(deftype writer () '(function (t t t fixnum) fixnum))

(defun pull-push-bytes (client in-fn out-fn)
  "Read data using IN-FN and write them out using OUT-FN.

Pass CLIENT as the first argument to both of them. the other are vector to read
or write and the size to read or write. Read function should return number of
bytes read.

Finish when the last read reads nothing.

Assumes writes cannot fail."
  (declare (optimize speed (safety 1) (debug 0))
           (reader in-fn)
           (writer out-fn))
  (let ((vec (make-array *default-buffer-size* :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent vec))
    (loop
      for read = nil then t
      for n fixnum = (funcall in-fn client vec *default-buffer-size*)
      while (plusp n)
      ;; assumption: never fail
      do
         (assert (= n (funcall out-fn client vec 0 n)))
      finally (return read))))

(defun pull-once-push-bytes (client in-fn out-fn)
  "Read data using IN-FN and write them out using OUT-FN.

Pass CLIENT as the first argument to both of them. the other are vector to read
or write and the size to read or write. Read function should return number of
bytes read.

Finish when the last read reads nothing.

Assumes writes cannot fail."
  (declare (optimize speed (safety 1) (debug 0))
           (reader in-fn)
           (writer out-fn))
  (let* ((vec (make-array *default-buffer-size* :element-type '(unsigned-byte 8)))
         (n (funcall in-fn client vec *default-buffer-size*)))
    (declare (dynamic-extent vec))
    (assert (= n (funcall out-fn client vec 0 n)))
    (= (the fixnum *default-buffer-size*) n)))

(defun push-bytes (client next-fn out-fn vector from to)
  "Process octets in the VECTOR in the interval <FROM TO).

Call OUT-FN on VECTOR, FROM TO. It returns number of processed octets (or 0 if
none, or raises an error). If something is written, NEXT-FN is also called to do \"next stage\".

Repeat on partial write."
  (if (= from to)
      from
      (loop
        for written = (funcall out-fn client vector from to)
        do
           (incf from written)
           (cond
             ((= from to)
              (funcall next-fn client written)
              (return from))
             ((plusp written)
              (funcall next-fn client written))
             ((zerop written)
              (return from))))))

(defun handle-ssl-errors (client ret)
  "Check real error after a call to SSL_connect, SSL_accept,
SSL_do_handshake, SSL_read_ex, SSL_read, SSL_peek_ex, SSL_peek, SSL_shutdown,
SSL_write_ex or SSL_write.

If the operation was successfully completed, do nothing.

If it is a harmless one (want read or want write), try to process the data.

Raise error otherwise."
  (handler-case (handle-ssl-errors* client ret)
    (ssl-wants-read () (remove-state client 'neg-bio-needs-read))))

(defun maybe-init-ssl (client)
  "If SSL is not initialized yet, initialize it."
  (cond
    ((zerop (ssl-is-init-finished (client-ssl client)))
     (handle-ssl-errors client (ssl-accept (client-ssl client))))
    (t (remove-state client 'ssl-init-needed)
       (add-state client 'can-read-bio))))

(defun doubled-buffer (buffer)
  "Return a larger buffer with same initial data as the provided one."
  (let ((new (make-array (* 2 (length buffer))
                         :element-type '(unsigned-byte 8))))
    (replace new buffer)
    new))

(defun double-buffer-size (old)
  "Analog of realloc."
  (let ((new (make-array (* 2 (length old))
                         :element-type '(unsigned-byte 8))))
    (replace new old)
    new))

(define-writer queue-encrypted-bytes write-buffer (client new-data from to)
;;  "Queue and possibly write encrypted data to write to the socket."
  (let ((max-buffer-size (length (client-write-buf client))))
    (cond
      ((> max-buffer-size (+ (client-write-buf-size client) (- to from)))
       ;; New data fit to the buffer
       (replace (client-write-buf client) new-data
                :start1 (client-write-buf-size client)
                :start2 from
                :end2 to)
       (incf (client-write-buf-size client) (- to from))
       (- to from))
      ((if-state client 'can-write)
       ;; There is too much data, but we can write to socket
       (replace (client-write-buf client) new-data
                :start1 (client-write-buf-size client)
                :start2 from)
       (incf from (- max-buffer-size from))
       (setf (client-write-buf-size client) max-buffer-size)
       (unless (= (write-data-to-socket client) max-buffer-size)
         (error 'http2-simple-error :format-control "Full write failed, followup not implemented (FIXME)"))
       (queue-encrypted-bytes client new-data from to)
       ;;
       (+ max-buffer-size (- to from))) ;; FIXME: this should be checked
      (t
       (error "FIXME: extend buffer ~s (used ~d) to accomodate ~d more and do proper stuff"
              (client-write-buf client) (client-write-buf-size client)
              (- to from))))))



(defun concatenate* (vectors)
  (let* ((len (reduce #'+ vectors :key #'length))
         (res (make-array len :element-type '(unsigned-byte 8))))
    (loop for v of-type (simple-array (unsigned-byte 8)) in vectors
          with i = 0
          do
             (setf (subseq res i (+ i (length v))) v)
             (incf i (length v))
          finally (return res))))

(defun set-next-action (client action size)
  "Set action for next chunk of received data."
  (setf (client-io-on-read client) action
        (client-octets-needed client) size))

(defconstant +client-preface-length+ (length http2/core:+client-preface-start+))

(defclass poll-server-connection (server-http2-connection
                                   routing-mixin)
  ((client :accessor get-client :initarg :client))
  (:default-initargs :stream-class 'vanilla-server-stream)
  (:documentation "The poll server connection has a client object and send data to it."))

(defmethod queue-frame ((connection poll-server-connection) frame)
  (with-slots (client) connection
    (unless (plusp (client-fd client))
      (error 'end-of-file :stream connection))
    (send-unencrypted-bytes client frame nil)))

(defmethod flush-http2-data ((connection poll-server-connection))
  (encrypt-and-send (get-client connection)))

(defun make-tls-endpoint (socket ctx application-data idx)
  "Make a generic instance of a TLS-ENDPOINT usable both for a client and for a server.

The read and write buffers are intitialized to new "
  (let* ((client (make-client% :fd socket
                               :fdset-idx idx
                               :octets-needed +client-preface-length+
                               :io-on-read #'parse-client-preface
                               ;; FIXME: use class from the dispatcher
                               :application-data application-data)))
    (init-tls-endpoint-core client ctx)
    client))

(defun make-client (&rest args) (apply #'make-tls-endpoint args))
(declaim (sb-ext:deprecated :early ("http2" "2.0.3")
                     (function make-client :replacement make-tls-endpoint)))

(defmacro with-tls-endpoint ((name context &key (fd -1) application-data (fdset-idx -1))
                             &body body)
  "Run BODY with NAME lexically bound to INIT, and it should be a TLS endpoint.

When control leaves the body, either normally or abnormally, The SSL of the
endpoint is freed.

The defaults reflect the fact that the macro should not be used with TLS endpoints used inside a FDSET."
  `(let ((,name (make-tls-endpoint ,fd ,context ,application-data ,fdset-idx)))
     (declare (type tls-endpoint ,name))
     (unwind-protect
          (progn ,@body)
       (close-openssl ,name))))

(defun make-tls-server-object (socket ctx idx connection-object)
  "Create new TLS ENDPOINT object suitable for TLS server.

Initially, the ENCRYPT-BUFFER contains the settings to send, and next action is
reading of the client hello."
  ;; FIXME: use class from the dispatcher
  (let* ((client (make-tls-endpoint socket ctx connection-object idx)))
    (setf (get-client (client-application-data client)) client) ;
    (ssl-set-accept-state (client-ssl client)) ; set as server; no return value
    (set-next-action client #'parse-client-preface +client-preface-length+)
    (write-settings-frame (client-application-data client) nil)
    client))

(defun handle-client-io (client dispatcher)
  (call-with-poll-info dispatcher (client-fdset-idx client)
                       (lambda (input-ready output-ready err-or-hup)
                         (when input-ready  (add-state client 'can-read-port))
                         (when output-ready (add-state client 'can-write))
                         (do-available-actions client)
                         (when err-or-hup
                           (unless (eql #'parse-frame-header (client-io-on-read client))
                             (warn "Poll error for ~a: ~d" client err-or-hup))
                           (signal 'done))
                         (and (if-state client 'has-data-to-write)
                              (not (if-state client 'can-write))))))

(defun setup-new-connect-pollfd (fdset listening-socket)
  (add-socket-to-fdset% fdset (sb-bsd-sockets:socket-file-descriptor (usocket:socket listening-socket))
                       0))

(defvar *nagle* t
  "If nil, disable Nagle algorithm (= enable nodelay)")

(defun add-socket-to-fdset (fdset socket client fd-idx)
  ;; TODO: rename to ADD-CLIENT-TO-FDSET
  (add-socket-to-fdset% fdset socket
   (setf (client-fdset-idx client) fd-idx)))

(defun process-new-client (listening-socket ctx dispatcher)
  "Add new client: accept connection, create client and add it to pollfd and to *clients*."
  (let* ((socket (checked-syscall #'plusp #'accept
                                  (sb-bsd-sockets:socket-file-descriptor (usocket:socket listening-socket)) (null-pointer) (null-pointer)))
         (fdset-idx (add-new-fdset-item socket dispatcher)))
    (when fdset-idx
      (push (make-tls-server-object socket ctx fdset-idx (make-connection-object dispatcher)) (get-clients dispatcher))
      (setup-port socket *nagle*)
      (format *error-output* "~&~A Connected~%"
              (get-ip (client-application-data (car (get-clients dispatcher)))))
      (force-output *error-output*))))

(defun maybe-process-new-client (listening-socket ctx dispatcher)
  "Add new client: accept connection, create client and add it to pollfd and to *clients*."
  (call-with-poll-info dispatcher 0
                       (lambda (in out error-or-end)
                         (declare (ignore out))
                         (cond
                           (error-or-end
                            (error "Error on listening socket"))
                           (in (process-new-client listening-socket ctx dispatcher)))))
  #+old
  (with-foreign-slots ((revents) fdset (:struct pollfd))
    (cond
      ((plusp (logand revents  (logior c-POLLERR  c-POLLHUP  c-POLLNVAL)))
       (error "Error on listening socket"))
      ((zerop (logand c-pollin revents))) ; no new client, do nothing
      (t (process-new-client listening-socket ctx dispatcher)))))

(defun close-client-connection (client dispatcher reason)
  (declare (ignore reason))
  (close-openssl client)
  (with-slots (clients) dispatcher
    (setf clients (remove client clients))
    (push (client-fdset-idx client) (http2/tcpip::get-empty-fdset-items dispatcher))
    (set-fd-slot (get-fdset dispatcher) -1 0 (client-fdset-idx client))
    (close-fd (client-fd client))
    (setf (client-fd client) -1)))

(define-condition poll-server-close ()
  ((dispatch :accessor get-dispatch :initarg :dispatch)
   (client   :accessor get-client   :initarg :client)
   (reason   :accessor get-reason   :initarg :reason)))

(defun process-client-sockets (nread dispatcher)
  (with-slots (clients) dispatcher
    (unless (zerop nread)
      (dolist (client clients)
        (restart-case
            (handler-case
                (handle-client-io client dispatcher)
              (communication-error (err) (invoke-restart 'http2/core:close-connection err))
;              (ssl-error-condition (err) (invoke-restart 'http2/core:close-connection err))
              (connection-error (err) (invoke-restart 'http2/core:close-connection err))) ; e.g., http/1.1 client
          (close-connection (&optional err)
            :report "Close current connection"
            (print err *error-output*)
            (force-output *error-output*)
            (unwind-protect
                 (when err
                   (signal 'poll-server-close :client client :dispatch dispatcher
                                              :reason err))
              (close-client-connection client dispatcher err))))))))

(define-condition poll-timeout (communication-error)
  ()
  (:documentation
   "Poll was called with a timeout and returned before any file descriptor became
ready."))

(defvar *no-client-poll-timeout* :∞
        "Default timeout in seconds to use when there is no client (to limit time to a client to
connect).

This is supposed to be used primarily in the automated tests, where you do not
want indefinite waits in case of a problem.

On timeout signals POLL-TIMEOUT error.

Default means an indefinite wait.")

(defvar *poll-timeout* :∞
  "Default timeout in seconds to use for poll when there are connected clients,
but no client communication.

On timeout signals POLL-TIMEOUT error.

Default means an indefinite wait.")

(defun compute-timeout (dispatcher)
  "Compute time for wait for any communication.

This is determined by lesser of timeout for ending the communication and
scheduler timeout.

Second value indicates whether the timeout should signal a condition (i.e., it
is not from scheduler)"
  (with-slots (fdset-size nagle poll-timeout no-client-poll-timeout) dispatcher
    (let*
        ((no-task-time (if (get-clients dispatcher) poll-timeout no-client-poll-timeout))
         (timeout (compute-poll-timeout-value no-task-time))
         (scheduler-time (round (* 1000.0 (time-to-action (car (get-scheduled-tasks *scheduler*)))))))
      (cond
        ((and (scheduler-empty-p *scheduler*) (eql no-task-time :∞))
         ;; Wait till next task available or forever.]
         (values -1 nil))
        ((eql no-task-time :∞)
         (values scheduler-time nil))
        ((scheduler-empty-p *scheduler*)
         (values timeout t))
        ((> scheduler-time timeout)
         (values timeout t))
        (t (values scheduler-time nil))))))


(defmacro with-clients ((dispatcher) &body body)
  `(unwind-protect
        (progn ,@body)
     (dolist (client (get-clients ,dispatcher))
       (close-client-connection client dispatcher "End of loop"))))

(defun serve-tls (listening-socket dispatcher)
  "Serve TLS communication on the LISTENING-SOCKET using DISPATCHER.

Establish appropriate context (clients list, fdset, TLS context, scheduler).

Handle new connections on LISTENING-SOCKET (creates new managed socket) and
managed sockets (read and write data, call client callback when data are
available), and run scheduled actions."
  (declare (optimize safety debug (speed 0))
           (type poll-dispatcher-mixin dispatcher))
  (with-fdset (dispatcher)
    (let ((*scheduler* (make-instance 'scheduler)))
      (with-ssl-context (ctx dispatcher)
        (with-clients (dispatcher)
          (setup-new-connect-pollfd (get-fdset dispatcher) listening-socket)
          (locally (declare (optimize speed (debug 1) (safety 1))
                            (ftype (function (t t) fixnum) poll))
            (loop
              (multiple-value-bind (timeout signal) (compute-timeout dispatcher)
                (let*
                    ((nread (poll dispatcher timeout)))
                  (run-mature-tasks *scheduler*)
                  (when (and (zerop nread) signal) (cerror "Ok" 'poll-timeout :medium "Server poll"))
                  (process-client-sockets nread dispatcher)
                  (maybe-process-new-client listening-socket ctx dispatcher))))))))))

(defun compute-poll-timeout-value (human-form)
  "Compute poll timeout from human readable value (seconds or :∞) to value accepted by
poll (miliseconds or -1)"
  (etypecase human-form
    ((eql :∞) -1)
    ((real 0) (round (* 1000 human-form)))))

(defclass poll-dispatcher-mixin (fdset-manager)
  ((poll-timeout           :accessor get-poll-timeout           :initarg :poll-timeout
                           :documentation "See *POLL-TIMEOUT*.")
   (no-client-poll-timeout :accessor get-no-client-poll-timeout :initarg :no-client-poll-timeout
                           :documentation "See *NO-CLIENT-POLL-TIMEOUT*.")
   (nagle                  :accessor get-nagle                  :initarg :nagle)
   (clients                :accessor get-clients                :initarg :clients))
  (:default-initargs :poll-timeout *poll-timeout*
                     :no-client-poll-timeout *no-client-poll-timeout*
                     :nagle *nagle*
                     :clients nil)
  (:documentation
   "Uses poll to listen to a set of clients and handle arriving packets in a single
thread.

Maximum number of clients is fixed (by default *fdset-size*, by default
10). Additional clients wait until one of existing client leaves.

Timeouts can be specified for polling."))

(defmethod print-object ((dispatcher poll-dispatcher-mixin) out)
  (print-unreadable-object (dispatcher out :type t)
    (format out "~a clients" (length (get-clients dispatcher)))))

(defclass poll-dispatcher (poll-dispatcher-mixin tls-dispatcher-mixin)
  ()
  (:default-initargs :connection-class 'poll-server-connection))

(defmethod get-no-client-poll-timeout :after ((dispatcher poll-dispatcher))
  (compute-poll-timeout-value (call-next-method)))

(defclass detached-poll-dispatcher (detached-server-mixin poll-dispatcher)
  ()
  (:documentation "Detached version of the POLL-DISPATCHER."))

(defmethod do-new-connection (socket (dispatcher poll-dispatcher-mixin))
  "Handle new connections by adding pollfd to and then polling.

When poll indicates available data, process them with openssl using BIO. Data to
the client are sent to SSL to BIO to socket buffer (and again poll to write
them).

This in the end does not use usocket, async nor cl+ssl - it is a direct rewrite
from C code."

  (let ((*nagle* (get-nagle dispatcher))
        (*scheduler* (make-instance 'scheduler)))

    (serve-tls socket dispatcher))
  ;; there is an outer loop in create-server that we want to skip
  (invoke-restart 'kill-server))



;;;; HTTP2 TLS async client
(define-condition not-enough-data (connection-error)
  ((expected :accessor get-expected :initarg :expected)
   (received :accessor get-received :initarg :received))
  (:default-initargs :code 0)
  (:documentation "Signalled when the SSL layer did not provide enough data to be processed by the
upper level. This can be fixed and buffered, but sane peers do not bring this situation on us."))

(defun on-complete-ssl-data (client)
  "Read number of octets indicated in CLIENT into a vector and then apply client fn on it.

The code assumes that single client request message is not split across several
TLS frames. If it does, well, connection error."
  (let* ((octets (client-octets-needed client))
         (vec (make-shareable-byte-vector octets)))
    ;; TODO: check first with SSL_pending?
    (let ((read (ssl-read client vec octets)))
      (cond
        ((not (plusp read)))
        ((/= read octets)
         (error 'not-enough-data :expected octets :received (subseq vec 0 read)
                :connection (client-application-data client)))
        (t (run-user-callback client vec))))))
