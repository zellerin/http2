(in-package #:http2/server/poll)


(mgl-pax:defsection  @async-server (:title "Polling server overview")
  #+nil  (client type)
  (@app-interface mgl-pax:section)
  (@request-handling mgl-pax:section))

(mgl-pax:defsection @app-interface (:title "Interface to the polling server")
  "POLL-DISPATCHER-MIXIN tracks a pool of clients that are connected to a
socket. Each client has a user registered callback function and required number
of octets to have available to call the callback. Callbacks call
SEND-UNENCRYPTED-BYTES to encrypt and send more data.

The central function is SERVE-TLS that orchestrates reading, decrypting,
encrypting and writing of data for all the sockets."
  (make-client function)
  (get-clients function)
  (client-application-data function)
  (client-ssl function)
  (poll-dispatcher-mixin class)
  (process-client-sockets function)
  (add-socket-to-fdset function)
  (send-unencrypted-bytes function)
  (encrypt-and-send function)
  (set-next-action function)
  (get-fdset-size (method nil (poll-dispatcher-mixin)))
  (get-poll-timeout (method nil (poll-dispatcher-mixin)))
  (get-no-client-poll-timeout (method nil (poll-dispatcher-mixin)))
  #+nil ((ssl-read function)
         (send-unencrypted-bytes function))
  ;; Timeouts
  (*no-client-poll-timeout* variable)
  (*poll-timeout* variable)
  (poll-timeout condition)
  (compute-poll-timeout-value function))

(mgl-pax:defsection @request-handling
    (:title "Client actions loop (implementation)"
     :export nil)
  "Each client has a STATE that encapsulates what actions are effectively possible.
SELECT-NEXT-ACTION selects appropriate action. When no new action is available,
next client is handled and eventually POLL called when all clients were served.

When POLL returns, new action is available for some client (read from a socket or write to it).

The actions are in general indicated by arrows in the diagram:

![](flow.svg)"

  (do-available-actions function)
  (select-next-action function)

  (process-data-on-socket function)
  (encrypt-data function)
  (move-encrypted-bytes function)
  (write-data-to-socket function))

(deftype buffer-size () `(integer 0 ,array-dimension-limit))
(declaim (buffer-size *default-buffer-size* *encrypt-buf-size*))

(defvar *encrypt-buf-size* 256
  "Initial size of the vector holding data to encrypt.")

(defvar *encrypted-buf-size* 1500
  "Initial size of the vector holding encrypted data")

(defvar *default-buffer-size* 1500) ; close to socket size


;;;; Async TLS endpoint state
(eval-when (:load-toplevel :compile-toplevel)
  (defparameter *states*
    '(CAN-READ-PORT           ; ①
      CAN-READ-SSL            ; ③
      CAN-WRITE-SSL           ; ④
      CAN-READ-BIO            ; ⑤
      CAN-WRITE               ; ⑥
      HAS-DATA-TO-WRITE       ; ⓤ
      HAS-DATA-TO-ENCRYPT     ; Ⓔ
      BIO-NEEDS-READ          ; B
      SSL-INIT-NEEDED         ; S
      PEER-CLOSED-CONNECTION) ; C
    "List of state bits that can a TLS endpoint have."))

(defun states-to-string (state)
  "Short string describing the state using codes on the diagram."
  (with-output-to-string (*standard-output*)
    (loop ;for state in *states*
          for state-idx from 0
          for label across "①③④⑤⑥ⓤⒺBSC"
          do (princ
              (if (plusp (ldb (byte 1 state-idx) state)) label #\Space)))))

(deftype state ()
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

(defvar *initial-state*
  (loop with state = 0
        for item in
                  '(CAN-WRITE CAN-WRITE-SSL bio-needs-read ssl-init-needed)
        do (setf (ldb (byte 1 (state-idx item)) state) 1)
        finally (return state)))



(defstruct (client  (:constructor make-client%)
                    (:print-object
                     (lambda (object out)
                       (format out "#<client fd ~d, ~d octets to ~a>" (client-fd object)
                               (client-octets-needed object) (client-io-on-read object)))))
  "Data of one client connection. This includes:

- File descriptor of underlying socket (FD),
- Opaque pointer to the openssl handle (SSL),
- Input and output BIO for exchanging data with OPENSSL (WBIO, RBIO),
- Plain text octets to encrypt and send (ENCRYPT-BUF),
- Encrypted octets to send to the file descriptor (WRITE-BUF),
- Callback function when read data are available (IO-ON-READ).
- Number of octets required by IO-ON-READ. Negative values have special handling.
- Client state from the low-level data flow point of view (STATE)
- Application data (slot to be used by the application)"
  (fd -1 :type fixnum)
  (ssl (null-pointer) :type cffi:foreign-pointer :read-only nil) ; mostly RO, but invalidated afterwards
  (rbio (null-pointer) :type cffi:foreign-pointer :read-only t)
  (wbio (null-pointer) :type cffi:foreign-pointer :read-only t)
  (write-buf (make-array *encrypted-buf-size* :element-type '(unsigned-byte 8))
   :type (and (simple-array (unsigned-byte 8))))
  (write-buf-size 0 :type fixnum)
  (encrypt-buf (make-array *encrypt-buf-size* :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8)))
  (io-on-read (constantly nil) :type compiled-function)
  (fdset-idx 0 :type fixnum :read-only nil) ; could be RO, but...
  (octets-needed 0 :type fixnum)
  (encrypt-buf-size 0 :type fixnum)
  (start-time (get-internal-real-time) :type fixnum)
  (state *initial-state* :type state)
  ;; set of CAN-READ-PORT, CAN-READ-SSL, HAS-DATA-TO-ENCRYPT, CAN-WRITE-SSL,
  ;; CAN-READ-BIO, HAS-DATA-TO-WRITE, CAN-WRITE
  ;; BIO-NEEDS-READ SSL-INIT-NEEDED
  application-data)

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
  (with-pointer-to-vector-data (buffer vec)
    (let ((read (read-2 (client-fd client) buffer vec-size)))
      (unless (= read vec-size)
        (remove-state client 'CAN-READ-PORT))
      (cond ((plusp read) read)
            ((zerop read)  (signal 'done))
            ((> -1 read) (error "This cannot happen #2"))
            ((/= (errno) EAGAIN)
             (warn "Read error (~d): ~d ~a" read (errno) (strerror (errno)))
             (signal 'done))
            (t 0)))))

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
  (unless
      (pull-once-push-bytes client #'read-from-peer
                              #'decrypt-socket-octets)
    (remove-state client 'can-read-port))
  (add-state client 'CAN-READ-SSL)
  (add-state client 'can-write-ssl)
  (when (if-state client 'bio-needs-read)
    (add-state client 'CAN-READ-BIO)
    (remove-state client
                  'BIO-NEEDS-READ)))

;;;; Read SSL
(defun ssl-read (client vec size)
   "Move up to SIZE octets from the decrypted SSL ③ to the VEC.

Return 0 when no data are available. Possibly remove CAN-READ-SSL flag."
   (let ((res
          (with-pointer-to-vector-data (buffer vec)
            (ssl-read% (client-ssl client) buffer size))))
     (unless (plusp res) (remove-state client 'can-read-ssl))
     (handle-ssl-errors client res)
     (max 0 res)))

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
    (assert (= processed (length new-data))))
  (add-state client 'has-data-to-encrypt))

;;;; Write to SSL
(define-writer encrypt-some output-ssl (client vector from to)
  (with-pointer-to-vector-data (buffer vector)
    (multiple-value-bind (res add remove)
        (encrypt-some* (client-ssl client) (inc-pointer buffer from) (- to from))
      (when add (add-state client add))
      (when remove (remove-state client remove))
      res)))

(defun encrypt-data (client)
  "Encrypt data in client's ENCRYPT-BUF.

Do nothing if there is no data to encrypt or SSL not yet initialized (and return zero).

Otherwise, use a temporary vector to write data "
  (push-bytes client
              (lambda (client written)
                (declare (ignore written))
                (setf (client-encrypt-buf-size client) 0)
                (remove-state client 'has-data-to-encrypt))
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
           (invoke-restart 'http2/core:close-connection)
           (error "Error during write: ~d (~a)" err (strerror err)))
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
            (t (error "Write failed"))))))

(defun encrypt-and-send (client)
  (unless (plusp (client-fd client))
    (error 'end-of-file :stream client))
  (encrypt-data client)
  (move-encrypted-bytes client)
  (write-data-to-socket client))

;;;; Action selector
(defun run-user-callback (client vec)
  "Run user defined callback and user return values to set next action."
  (multiple-value-call #'set-next-action client
    (funcall (client-io-on-read client) (client-application-data client) vec)))

(defun select-next-action (client)
  "One of possible next actions consistent with then the state of the client, or
nil if no action is available.

This is factored out so that it can be traced. There is a
TLS-SERVER/MEASURE::ACTIONS clip on this function."
  (cond
    ((if-state client 'can-read-port) #'process-data-on-socket)
    ((and (if-state client 'ssl-init-needed)
          (not (if-state client 'bio-needs-read)))
     #'maybe-init-ssl)
    ((if-state client 'can-read-ssl)
     (if (plusp (client-octets-needed client))
         #'on-complete-ssl-data
         #'run-user-callback))
    ((and (if-state client 'has-data-to-encrypt)
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
  (let ((ssl (client-ssl client))
        (wbio (client-rbio client)))
    (let ((new-state (handle-ssl-errors* ssl wbio ret)))
      (when new-state
        (add-state client new-state)))))

(defun ssl-err-reason-error-string (code)
  (foreign-string-to-lisp (err-reason-error-string code)))

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
  (cond
    ((> (length (client-write-buf client)) (+ (client-write-buf-size client) (- to from)))
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
     (incf from (- (length (client-write-buf client)) from))
     (write-data-to-socket client)
     (queue-encrypted-bytes client new-data from to)
     ;;
     (+ (length (client-write-buf client)) (- to from))) ;; FIXME: this should be checked
    (t
     (error "FIXME: extend buffer ~s (used ~d) to accomodate ~d more and do proper stuff"
            (client-write-buf client) (client-write-buf-size client)
            (- to from)))))

(define-condition done (error)
  ()
  (:documentation "The socket on the other side is closed."))

(define-condition ssl-error-condition (error)
  ((code :accessor get-code :initarg :code))
  (:documentation "The socket on the other side is closed."))

(defmethod print-object ((object ssl-error-condition) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~x: ~a" (get-code object)
            (ssl-err-reason-error-string (get-code object)))))

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

(defun make-client (socket ctx application-data idx)
  "Make a generic instance of a CLIENT usable both for a client and for a server.

The read and write buffers are intitialized to new  "
  (let* ((s-mem (bio-s-mem))
         (client (make-client% :fd socket
                               :rbio (bio-new s-mem)
                               :wbio (bio-new s-mem)
                               :ssl (ssl-new ctx)
                               :fdset-idx idx
                               :octets-needed +client-preface-length+
                               :io-on-read #'parse-client-preface
                               ;; FIXME: use class from the dispatcher
                               :application-data application-data)))
    (ssl-set-bio (client-ssl client) (client-rbio client) (client-wbio client))
    client))

(defun make-client-object (socket ctx idx)
  "Create new CLIENT object suitable for TLS server.

Initially, the ENCRYPT-BUFFER contains the settings to send, and next action is
reading of client hello."
  ;; FIXME: use class from the dispatcher
  (let* ((client (make-client socket ctx (make-instance 'poll-server-connection) idx)))
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
      (push (make-client-object socket ctx fdset-idx) (get-clients dispatcher))
      (setup-port socket *nagle*))))

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

(defun close-client-connection (client dispatcher)
  (with-slots (clients) dispatcher
    (setf clients (remove client clients))
    (unless (null-pointer-p (client-ssl client))
      (ssl-free (client-ssl client)))   ; BIOs are closed automatically
    (setf (client-ssl client) (null-pointer))
    (push (client-fdset-idx client) (http2/tcpip::get-empty-fdset-items dispatcher))
    (set-fd-slot (get-fdset dispatcher) -1 0 (client-fdset-idx client))
    (close-fd (client-fd client))
    (setf (client-fd client) -1)))

(defun process-client-sockets (nread dispatcher)
  (with-slots (clients) dispatcher
    (unless (zerop nread)
      (dolist (client clients)
          (restart-case
              (handler-case
                  (handle-client-io client dispatcher)
                (done () (invoke-restart 'http2/core:close-connection))
                #+too-early-and-loses-info
                (ssl-error-condition () (invoke-restart 'http2/core:close-connection))
                (connection-error () (invoke-restart 'http2/core:close-connection))) ; e.g., http/1.1 client
            (http2/core:close-connection ()
              (close-client-connection client dispatcher)))))))

(define-condition poll-timeout (error)
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
       (close-client-connection client dispatcher))))

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
                  (when (and (zerop nread) signal) (cerror "Ok" 'poll-timeout))
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

(defclass poll-dispatcher (poll-dispatcher-mixin tls-dispatcher-mixin base-dispatcher)
  ())

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

  (let ((*nagle* (get-nagle dispatcher)))
    (serve-tls socket dispatcher))
  ;; there is an outer loop in create-server that we want to skip
  (invoke-restart 'kill-server))



;;;; HTTP2 TLS async client
(define-condition not-enough-data (connection-error)
  ((expected :accessor get-expected :initarg :expected)
   (received :accessor get-received :initarg :received))
  (:default-initargs :code 0)
  (:documentation "Signalled when the SSL layer did not provide enough data to be processed bby the
upper level. This can be fixed and buffered, but sane clients do not do that."))

(defun on-complete-ssl-data (client)
  "Read number of octets indicated in CLIENT into a vector and then apply client fn on it.

 FIXME: process that anyway"
  (let* ((octets (client-octets-needed client))
         (vec (make-shareable-byte-vector octets)))
    ;; TODO: check first with SSL_pending?
    (let ((read (ssl-read client vec octets)))
      (cond
        ((not (plusp read))
         (handle-ssl-errors client read))
        ((/= read octets)
         (error 'not-enough-data :expected octets :received (subseq vec 0 read)
                :connection (client-application-data client)))
        (t (run-user-callback client vec))))))
