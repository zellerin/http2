(in-package #:http2/server/poll)

(defsection @poll-server (:title "STDIO basic API" :export nil)
  (fcntl function)
  (poll% function)
  (close-fd function)
  (errno function)
  (read function)
  (accept function)
  (setsockopt function)
  (strerror function))

#-os-macosx(defcfun ("__errno_location" errno%) :pointer)
#+os-macosx(defcfun ("__error" errno%) :pointer)
(defcfun ("strerror_r" strerror-r%) :pointer
  "System error message for error number" (errnum :int) (buffer :pointer) (buflen :int))

(defcfun ("poll" poll%) :int "Synchronous I/O multiplexing. Called by POLL."
  (fdset :pointer) (rb :int) (timeout :int))
(defcfun ("close" close-fd) :int
    "See close(2). The Lisp name is changed so that not to cause a conflict." (fd :int))
(defcfun ("read" read-2) :int (fd :int) (buf :pointer) (size :int))
(defcfun ("write" write-2) :int (fd :int) (buf :pointer) (size :int))
(defcfun "fcntl" :int "See man fcntl(2). Used to set the connection non-blocking."
  (fd :int) (cmd :int) (value :int))
(defcfun "accept" :int (fd :int) (addr :pointer) (addrlen :int))
(defcfun "setsockopt" :int "See man setsockopt(2). Optionally used to switch Nagle algorithm."
  (fd :int) (level :int) (optname :int) (optval :pointer) (optlen :int))

(defun strerror (errnum)
  "Lisp string for particular error. See man strerror(3)."
  (let ((str (make-array 256 :element-type 'character)))
    (with-pointer-to-vector-data (buffer str)
      (foreign-string-to-lisp (strerror-r% errnum buffer 256)))))

(defun errno ()
  "See man errno(3). "
  (mem-ref (errno%) :int))


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
  (poll-dispatcher-mixin class)
  (send-unencrypted-bytes function)
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

  (process-client-fd function)
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

(defvar *default-buffer-size* 1500) ; close to socket size

(defstruct (client  (:constructor make-client%)
                    (:print-object
                     (lambda (object out)
                       (format out "#<client fd ~d, ~d octets to ~a>" (client-fd object)
                               (client-octets-needed object) (client-io-on-read object)))))
  "Data of one client connection. This includes:

- File descriptor of underlying socket (FD),
- Opaque pointer to the openssl handle (SSL),
- Input and output BIO for exchanging data with OPENSSL (RBIO, WBIO),
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
  (write-buf nil :type (or null cons))
  (encrypt-buf (make-array *encrypt-buf-size* :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8)))
  (io-on-read (constantly nil) :type compiled-function)
  (fdset-idx 0 :type fixnum :read-only nil) ; could be RO, but...
  (octets-needed (length +client-preface-start+) :type fixnum)
  (encrypt-buf-size 0 :type fixnum)
  (start-time (get-internal-real-time) :type fixnum)
  (state (list 'CAN-WRITE 'CAN-WRITE-SSL 'bio-needs-read 'ssl-init-needed))
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


;;;; Client state
(defun if-state (client state)
  (member state (client-state client)))

(defun add-state (client state)
  (pushnew state (client-state client)))

(defun remove-state (client state)
  (setf (client-state client)
        (remove state (client-state client))))

(defun states-to-string (state)
  "Short string describing the state using codes on the diagram."
  (with-output-to-string (*standard-output*)
    (when (member 'can-read-port state)
      (princ #\①))
    (when (member 'can-read-ssl state)
      (princ #\③))
    (when (member 'has-data-to-write state)
      (princ #\ⓤ))
    (when (member 'can-write-ssl state)
      (princ #\④))
    (when (member 'can-read-bio state)
      (princ #\⑤))
    (when (member 'has-data-to-write state)
      (princ #\Ⓔ))
    (when (member 'can-write state)
      (princ #\⑥))))



;;;; Input port

(defun setup-port (socket nagle)
  "Set the TCP socket: nonblock and possibly nagle."
  (unless (zerop (fcntl socket f-setfl
                        (logior o-nonblock (fcntl socket f-getfl 0))))
    (error "Could not set O_NONBLOCK on the client"))
  (unless nagle
    (unless (zerop
             (with-foreign-object (flag :int)
               (setf (mem-ref flag :int) 1)
               (setsockopt socket ipproto-tcp tcp-nodelay
                           flag (foreign-type-size :int))))
      (error "Could not set O_NODELAY on the client"))))

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
   nil
   (let ((res
          (with-pointer-to-vector-data (buffer vec)
            (ssl-read% (client-ssl client) buffer size))))
     (unless (plusp res) (remove-state client 'can-read-ssl))
     (handle-ssl-errors client res)
     (max 0 res)))

;;;; Encrypt queue
(defun send-unencrypted-bytes (client new-data comment)
  "Collect new data to be encrypted and sent to client.

Data are just concatenated to the ENCRYPT-BUF. Later, they would be encrypted
and passed."
  (declare (ignore comment))
  (let ((old-fp (client-encrypt-buf-size client)))
    (setf (client-encrypt-buf-size client) (+ old-fp (length new-data)))
    (loop
      while (> (client-encrypt-buf-size client) (length (client-encrypt-buf client)))
      do (setf (client-encrypt-buf client)
               (double-buffer-size (client-encrypt-buf client))))
    (replace (client-encrypt-buf client) new-data :start1 old-fp))
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
  (with-pointer-to-vector-data (buffer vector)
    (let ((res (write-2 (client-fd client)
                        (inc-pointer buffer from)
                        (- to from)))
          (err (errno)))
      (cond ((and (= res -1)
                  (= err eagain))
             (remove-state client 'can-write)
             0)
            ((= res -1)
             ;; e.g., broken pipe
             (invoke-restart 'http2/core:close-connection)
             (error "Error during write: ~d (~a)" err (strerror err)))
            ((plusp res) res)
            (t (error "This cant happen (#1)"))))))

(defun write-data-to-socket (client)
  "Write buffered encrypted data Ⓔ to the client socket ⑥. Update the write buffer to
keep what did not fit."
  (let ((concated (concatenate* (client-write-buf client)))) ;;DEBUG
    (let ((written
            (push-bytes client (constantly nil)
                         #'send-to-peer
                         concated 0 (length concated))))
      (setf (client-write-buf client)
            (cond ((= written (length concated))
                   (remove-state client 'has-data-to-write)
                   nil)
                  ((plusp written)
                   (remove-state client 'can-write)
                   (list (subseq concated written)))
                  (t (error "Write failed")))))))

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
    ((and (if-state client 'ssl-init-needed)
          (not (if-state client 'bio-needs-read)))
     #'maybe-init-ssl)
    (t nil)))

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
      (add-state client new-state))))

(defun ssl-err-reason-error-string (code)
  (foreign-string-to-lisp (err-reason-error-string code)))

(defun maybe-init-ssl (client)
  "If SSL is not initialized yet, initialize it."
  (if (zerop (ssl-is-init-finished (client-ssl client)))
    (handle-ssl-errors client (ssl-accept (client-ssl client)))
    (remove-state client 'ssl-init-needed)))

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
  (setf (client-write-buf client)
        (append (client-write-buf client)
                (list (subseq new-data from to))))
  (- to from))

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

(defun process-client-fd (fd-ptr client)
  "Process events available on FD-PTR (a pointer to struct pollfd) with CLIENT.

The new possible action corresponding to ① or ⑥ on the diagram above is added to the client state and DO-AVAILABLE-ACTIONS is called to react to that."
  (with-foreign-slots ((fd events revents) fd-ptr (:struct pollfd))
    (when (plusp (logand c-pollin revents)) (add-state client 'can-read-port))
    (when (plusp (logand c-pollout revents)) (add-state client 'can-write))
    (do-available-actions client)
    (when (plusp (logand revents (logior c-POLLERR  c-POLLHUP  c-POLLNVAL)))
      ;; this happens, e.g., with h2load -D (time based)
      (unless (eql #'parse-frame-header (client-io-on-read client))
        (warn "Poll error for ~a: ~d" client (logand revents (logior c-POLLERR  c-POLLHUP  c-POLLNVAL))))
      (signal 'done))))

(defvar *fdset-size* 10
  "Size of the fdset - that, is, maximum number of concurrent clients.")

#+nil
(defvar *empty-fdset-items* nil "List of empty slots in the fdset table.")

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

(defun make-client (socket ctx application-data)
  "Make a generic instance of a CLIENT usable both for a client and for a server.

The read and write buffers are intitialized to new  "
  (let* ((s-mem (bio-s-mem))
         (client (make-client% :fd socket
                               :rbio (bio-new s-mem)
                               :wbio (bio-new s-mem)
                               :ssl (ssl-new ctx)
                               :octets-needed +client-preface-length+
                               ;; FIXME: use class from the dispatcher
                               :application-data application-data)))
    (ssl-set-bio (client-ssl client) (client-rbio client) (client-wbio client))
    client))

(defun make-client-object (socket ctx)
  "Create new CLIENT object suitable for TLS server.

Initially, the ENCRYPT-BUFFER contains the settings to send, and next action is
reading of client hello."
  ;; FIXME: use class from the dispatcher
  (let* ((client (make-client socket ctx (make-instance 'poll-server-connection))))
    (setf (get-client (client-application-data client)) client) ;
    (ssl-set-accept-state (client-ssl client)) ; set as server; no return value
    (set-next-action client #'parse-client-preface +client-preface-length+)
    (write-settings-frame (client-application-data client) nil)
    client))

(defun set-fd-slot (fdset socket new-events idx)
  "Set FD and EVENTS of slot IDX in fdset."
  (with-foreign-slots ((fd events)
                       (inc-pointer fdset (* idx size-of-pollfd))
                       (:struct pollfd))
    (when socket (setf fd socket events new-events))))

(defun add-socket-to-fdset (fdset socket client fd-idx)
  "Find free slot in the FDSET and put client there."
  (set-fd-slot fdset socket  (logior c-pollerr c-pollhup c-pollnval c-pollin) fd-idx )
  (setf (client-fdset-idx client) fd-idx))

(defun init-fdset (fdset size)
  (loop for i from 0 to (1- size)
        do
           (set-fd-slot fdset -1 0 i)))

(defun handle-client-io (client fdset)
  (let ((fd-ptr (inc-pointer fdset (* (client-fdset-idx client) size-of-pollfd))))
    (process-client-fd fd-ptr client)
    (with-foreign-slots ((events)
                         fd-ptr
                         (:struct pollfd))
      (setf events
            (if (and (if-state client 'has-data-to-write)
                     (not (if-state client 'can-write)))
                (logior events c-pollout)
                (logand events (logxor -1 c-pollout)))))))

(defun setup-new-connect-pollfd (fdset listening-socket)
  (set-fd-slot fdset
               (sb-bsd-sockets:socket-file-descriptor (usocket:socket listening-socket))
                (logior c-pollerr c-pollhup c-pollnval c-pollin)
                0))

(defvar *nagle* t
  "If nil, disable Nagle algorithm (= enable nodelay)")

#+nil
(defvar *clients* nil
  "List of clients processed.")

(defun process-new-client (listening-socket ctx dispatcher)
  "Add new client: accept connection, create client and add it to pollfd and to *clients*."
  (with-slots (fdset empty-fdset-items clients) dispatcher
    (cond
      (empty-fdset-items
       (let* ((socket (accept
                       (sb-bsd-sockets:socket-file-descriptor (usocket:socket listening-socket)) (null-pointer) 0))
              (client-id (pop empty-fdset-items))
              (client (when client-id (make-client-object socket ctx))))
         (setup-port socket *nagle*)
         (add-socket-to-fdset fdset socket client client-id)
         (push client clients)))
      (t (warn 'http2-simple-warning :format-control "No place in fdset for the new client. Accepting and immediately closing it.")
         ;; Let the server catch it and
         (close-fd (accept
                    (sb-bsd-sockets:socket-file-descriptor (usocket:socket listening-socket)) (null-pointer) 0))))))

(defun maybe-process-new-client (listening-socket ctx dispatcher)
  "Add new client: accept connection, create client and add it to pollfd and to *clients*."
  (with-slots (fdset empty-fdset-items clients) dispatcher
    (with-foreign-slots ((revents) fdset (:struct pollfd))
      (cond
        ((plusp (logand revents  (logior c-POLLERR  c-POLLHUP  c-POLLNVAL)))
         (error "Error on listening socket"))
        ((zerop (logand c-pollin revents))) ; no new client, do nothing
        (t (process-new-client listening-socket ctx dispatcher))))))

(defun close-client-connection (client dispatcher)
  (with-slots (clients fdset) dispatcher
    (setf clients (remove client clients))
    (unless (null-pointer-p (client-ssl client))
      (ssl-free (client-ssl client)))   ; BIOs are closed automatically
    (setf (client-ssl client) (null-pointer))
    (push (client-fdset-idx client) (get-empty-fdset-items dispatcher))
    (set-fd-slot fdset -1 0 (client-fdset-idx client))
    (close-fd (client-fd client))
    (setf (client-fd client) -1)))

(defun process-client-sockets (nread dispatcher)
  (with-slots (fdset clients) dispatcher
    (unless (zerop nread)
      (dolist (client clients)
          (restart-case
              (handler-case
                  (handle-client-io client fdset)
                (done () (invoke-restart 'http2/core:close-connection))
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

(defun call-with-fdset (dispatcher fn)
  "Helper for WITH-FDSET."
  (with-slots (fdset-size fdset empty-fdset-items) dispatcher
    (with-foreign-object (fdset* '(:struct pollfd) fdset-size)
      (setf fdset fdset*)
      (init-fdset fdset fdset-size)
      (setf empty-fdset-items (alexandria:iota (1- fdset-size) :start 1))
      (unwind-protect
           (funcall fn)
        (setf fdset nil)))))

(defmacro with-fdset ((dispatcher) &body body)
  "Run BODY with FDSET slot of DISPATCHER set to a set of file descriptors for
poll (of size FDSET-SIZE, another DISPATCHER slot).

Initialize FDSET. Set EMPTY-FDSET-ITEMS slot of the dispatcher to all slot
items.

Cleanup FDSET after BODY is run."
  `(call-with-fdset ,dispatcher
                    (lambda () ,@body)))

(defun poll (dispatcher timeout)
  (with-slots (fdset fdset-size) dispatcher
      (poll% fdset fdset-size timeout)))

(defmacro with-clients ((dispatcher) &body body)
  `(unwind-protect
        ,@body
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
    (setup-new-connect-pollfd (get-fdset dispatcher) listening-socket)
    (let ((*scheduler* (make-instance 'scheduler)))
      (with-ssl-context (ctx dispatcher)
        (with-clients (dispatcher)
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

(defclass poll-dispatcher-mixin ()
  ((fdset-size             :accessor get-fdset-size             :initarg :fdset-size
                           :documentation "Number of slots for clients to be polled.")
   (poll-timeout           :accessor get-poll-timeout           :initarg :poll-timeout
                           :documentation "See *POLL-TIMEOUT*.")
   (no-client-poll-timeout :accessor get-no-client-poll-timeout :initarg :no-client-poll-timeout
                           :documentation "See *NO-CLIENT-POLL-TIMEOUT*.")
   (nagle                  :accessor get-nagle                  :initarg :nagle)
   (clients                :accessor get-clients                :initarg :clients)
   (fdset                  :accessor get-fdset                  :initarg :fdset)
   (empty-fdset-items      :accessor get-empty-fdset-items      :initarg :empty-fdset-items))
  (:default-initargs :fdset-size *fdset-size* :poll-timeout *poll-timeout*
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
    (format out "~a/~a clients" (length (get-clients dispatcher)) (get-fdset-size dispatcher))))

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
         (error "Read ~d octets. This is not enough octets, why?~%~s~%" read (subseq vec 0 read)))
        (t (run-user-callback client vec))))))

;;;; Sandbox
(defcfun socket :int "S"
  (domain :int) (type :int) (protocol :int))

(defun socketpair ()
  (with-foreign-object (pair :int 2)
    (let ((res (socketpair% 1 1  pair)))
      (unless (zerop res)
        (error "Socketpair failed: ~d" (errno))))
    (values (mem-aref pair :int 0 )
            (mem-aref pair :int 1 ))))

(defun call-with-tcp-pair (fn)
  (let* ((a (socket pf-inet sock-stream 0))
         (b (socket pf-inet sock-stream 0)))
    (unwind-protect
         (unwind-protect
              (dolist (socket (list a b))
                (unless (zerop (fcntl socket f-setfl
                                      (logior o-nonblock (fcntl socket f-getfl 0))))
                  (error "Could not set O_NONBLOCK on the client"))
                (unless (plusp (logand o-nonblock (fcntl socket f-getfl 0)))
                  (error "O_NONBLOCK on the client did not stick")))

           #+nil          (progn
                               (sb-bsd-sockets:socket-bind a)
                               (sb-bsd-sockets:socket-bind b)
                               (sb-thread:make-thread (lambda () (sb-bsd-sockets:socket-connect a #(127 0 0 1) (nth-value 1 (sb-bsd-sockets:socket-name b)))))
                               (sb-bsd-sockets:socket-connect b #(127 0 0 1) (nth-value 1 (sb-bsd-sockets:socket-name a)))
                               (funcall fn a b))
           (close-fd a))
      (close-fd b))))

(defmacro with-tcp-pair ((a b) &body body)
  (alexandria:with-gensyms (as bs)
    `(multiple-value-bind (,as ,bs) (make-tcp-pair)
       (let ((,a (sb-bsd-sockets:socket-file-descriptor ,as))
             (,b (sb-bsd-sockets:socket-file-descriptor ,bs)))
         (unwind-protect
              ,@body
)))))

(defun test ()
  (let ((dispatcher (make-instance 'poll-dispatcher-mixin :fdset-size 2)))
    (with-fdset (dispatcher)
      (with-ssl-context (ctx dispatcher)
        (with-tcp-pair (server-socket client-socket)
          (dolist (socket (list client-socket server-socket))
            (unless (zerop (fcntl socket f-setfl
                                  (logior o-nonblock (fcntl socket f-getfl 0))))
              (error "Could not set O_NONBLOCK on the client"))
            (unless (plusp (logand o-nonblock (fcntl socket f-getfl 0)))
              (error "Could not set O_NONBLOCK on the client")))
          (unwind-protect
               (let ((client (make-client client-socket ctx nil))
                     (server (make-client server-socket ctx nil)))

                 (print client)
                 (print server)
                 (set-next-action client #'print-data 10)
                 (set-next-action server #'print-data 10)
                 (setf (get-clients dispatcher) (list server client))
                 (with-slots (fdset) dispatcher
                   (ssl-accept (client-ssl server))
                   (ssl-connect (client-ssl client))
                   (add-socket-to-fdset fdset server-socket client 0)
                   (add-socket-to-fdset fdset client-socket server 1)
                   (send-unencrypted-bytes client (make-octet-buffer 10) nil)
                   (send-unencrypted-bytes server (make-octet-buffer 10) nil)
                   (encrypt-and-send client)
                   (encrypt-and-send server)
                   (dotimes (i 10)
                     (let ((nread (poll dispatcher 0)))
                       (process-client-sockets nread dispatcher)))))
            (close-fd server-socket)
            (close-fd client-socket)))))))
