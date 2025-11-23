(in-package #:http2/tcpip)

(defsection @tcpip-syscalls (:title "STDIO basic API" :export t)
  ""
  (fcntl function)
  (set-nonblock function)
  (setsockopt function)
  (set-nodelay function)
  (close-fd function) ;  implicit within call- and similar
  (write-buffer* function)
  (read-buffer function)
  (read-socket* function)
  (accept function)

  (with-socket macro)
  (call-with-tcp-pair function)
  (with-tcp-pair macro)
  "# Error handling support:"
  (errno function)
  (strerror function)
  (checked-syscall function)
  (syscall-error condition)
  (nonneg-or-eagain function)
  (eagain constant)

  (fd-to-ip function)
                                        ;  (sockaddr-in type)
  #+nil  (size-of-sockaddr-in constant)

  "File descriptor sets and polling

DISPATCHER should be something with slots ...

```
(with-fdset (dispatcher)
  (add-socket-to-fdset% ...)
  ...
  (unwind-protect
     (loop
        (poll ...)
        (handle-communication-for-fds))
  (make-sure-all-sockets are-closed-if-leaving)
```"
  (fdset-manager class)
  (with-fdset macro)
  (poll function)
  (set-fd-slot function)
  (add-socket-to-fdset% function)
  (call-with-fdset function)
  (call-with-poll-info function)
  (get-fdset generic-function)
  (get-fdset (method (fdset-manager)))
  (add-new-fdset-item function)
  (socket-bind function)
  (af-inet constant)
  (sock-stream constant))

(define-condition syscall-error (communication-error)
  ((call  :accessor get-call  :initarg :call)
   (errno :accessor get-errno :initarg :errno))
  (:documentation
   "Syscall failed. The printed form looks like
```
(socket-bind -1 0)
.. debugger invoked on SYSCALL-ERROR:
..   #<SYSCALL-ERROR 9 (Bad file descriptor) on -1 during #<FUNCTION BIND>>
```"))

(define-condition this-cant-happen (communication-error)
  ((where :accessor get-where :initarg :where)))

(defmethod print-object ((err syscall-error) out)
  (print-unreadable-object (err out :type t)
    (with-slots (errno call) err
      (format out "~d (~a) on ~a during ~s" errno (strerror errno)
              (http2/utils:get-medium err) call))))

(defun checked-syscall (check call &rest params)
  "Return the result value of the syscall when check passes.

Otherwise signal an error."
  (let ((res (apply call params)))
    (unless (funcall check res)
      (error 'syscall-error :medium (car params) :errno (errno) :call call))
    res))

#-os-macosx(defcfun ("__errno_location" errno%) :pointer)
#+os-macosx(defcfun ("__error" errno%) :pointer)
(defcfun (#+linux "strerror_r" #-linux "strerror" strerror-r%) :pointer
  "System error message for error number" (errnum :int) (buffer :pointer) (buflen :int))

(defcfun ("close" close-fd) :int
    "See close(2). The Lisp name is changed so that not to cause a conflict." (fd :int))
(defcfun ("read" read-2) :int (fd :int) (buf :pointer) (size :int))
(defcfun ("write" write-2) :int (fd :int) (buf :pointer) (size :int))
(defcfun "fcntl" :int "See man fcntl(2). Used to set the connection non-blocking."
  (fd :int) (cmd :int) (value :int))
(defcfun "accept" :int (fd :int) (addr :pointer) (addrlen :pointer))
(defcfun "bind" :int (fd :int) (addr :pointer) (addrlen :int))
(defcfun "setsockopt" :int "See man setsockopt(2). Optionally used to switch Nagle algorithm."
  (fd :int) (level :int) (optname :int) (optval :pointer) (optlen :int))
(defcfun "ioctl" :int "Use to set nonblock on OSX"
  (fd :int) (code :uint) (flag :pointer) (fsize :int))

(defcfun "getpeername" :int (socket :int) (address :pointer) (len :pointer))
(defcfun "inet_ntoa" :string (in :pointer))
(defcfun "htons" :ushort (port :ushort))

(defun strerror (errnum)
  "Lisp string for particular error. See man strerror(3)."
  (let ((str (make-array 256 :element-type 'character)))
    (with-pointer-to-vector-data (buffer str)
      (foreign-string-to-lisp (strerror-r% errnum buffer 256)))))

(defun errno ()
  "See man errno(3). "
  (mem-ref (errno%) :int))

;;;; Polling stuff
(defcfun ("poll" poll%) :int "Synchronous I/O multiplexing. Called by POLL."
  (fdset :pointer) (rb :int) (timeout :int))

(defun poll (dispatcher timeout)
  (with-slots (fdset fdset-size) dispatcher
    (checked-syscall (lambda (res) (or (not (minusp res)) (= eintr (errno))))
                     #'poll% fdset fdset-size timeout)))

(defun set-fd-slot (fdset socket new-events idx)
  "Set FD and EVENTS of slot IDX in fdset."
  (with-foreign-slots ((fd events)
                       (inc-pointer fdset (* idx size-of-pollfd))
                       (:struct pollfd))
    (when socket (setf fd socket events new-events))))

(defun init-fdset (fdset size)
  (loop for i from 0 to (1- size)
        do (set-fd-slot fdset -1 0 i)))

(defun add-socket-to-fdset% (fdset socket fd-idx)
  ;; TODO: rename to ADD-SOCKET-TO-FDSET when the name is free
  (set-fd-slot fdset socket  (logior c-pollerr c-pollhup c-pollnval c-pollin) fd-idx))

(defvar *fdset-size* 10
  "Size of the fdset - that, is, maximum number of concurrent clients.")

(defclass fdset-manager ()
  ((fdset-size             :accessor get-fdset-size             :initarg :fdset-size
                           :documentation "Number of slots for clients to be polled.")
   (fdset                  :accessor get-fdset                  :initarg :fdset)
   (empty-fdset-items      :accessor get-empty-fdset-items      :initarg :empty-fdset-items))
  (:default-initargs :fdset-size *fdset-size* :clients nil)
  (:documentation
   "Uses poll to listen to a set of clients and handle arriving packets in a single
thread.

Maximum number of clients is fixed (by default *fdset-size*, by default
10). Additional clients wait until one of existing client leaves.

Timeouts can be specified for polling."))

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

Cleanup FDSET - including closing sockets - after BODY is run."
  `(call-with-fdset ,dispatcher
                    (lambda () ,@body)))

(defun call-with-poll-info (dispatcher fd-idx fn)
  "Call FN with three parameters, INPUT-READY, OUTPUT-READY ERR-OR-HUP. It should
process the available input and return whether there is
a pending write."
  (with-slots (fdset) dispatcher
    (let ((fd-ptr (inc-pointer fdset (* fd-idx size-of-pollfd))))
      (with-foreign-slots ((fd events revents) fd-ptr (:struct pollfd))
        (setf events
              (if (funcall fn (plusp (logand c-pollin revents)) (plusp (logand c-pollout revents))
                           (when (plusp (logand revents (logior c-POLLERR  c-POLLHUP  c-POLLNVAL)))
                             (logand revents (logior c-POLLERR  c-POLLHUP  c-POLLNVAL))))
                  (logior events c-pollout)
                  (logand events (logxor -1 c-pollout))))))))

(defun add-new-fdset-item (socket dispatcher)
  "Add new socket to the fdset, and "
  (with-slots (fdset empty-fdset-items) dispatcher
    (cond
      (empty-fdset-items
       (let* ((client-id (pop empty-fdset-items)))
         (add-socket-to-fdset% fdset socket client-id)
         client-id))
      (t (warn 'http2-simple-warning :format-control "No place in fdset for the new client. Accepting and immediately closing it.")
         ;; Let the server catch it and
         (close-fd socket)))))



(defun socket-bind (socket &optional (host #x0100007f) (port 0))
  (with-foreign-object (addr '(:struct sockaddr-in))
    (with-foreign-slots ((sin-family sin-port sin-addr) addr (:struct sockaddr-in))
      (setf sin-family af-inet sin-port port sin-addr host)
      (checked-syscall #'zerop #'bind socket addr size-of-sockaddr-in)))
  socket)

(defun set-nonblock (socket)
  "Set socket as nonblocking. This is done usually by O_NONBLOCK on fcntl, however,
this failed silently on OSX.

So this uses ioctl and fionbio."

#-darwin
  (checked-syscall #'zerop #'fcntl socket f-setfl
                   (logior o-nonblock (checked-syscall #'plusp #'fcntl socket f-getfl 0)))
#-darwin
  (unless (plusp (logand o-nonblock (checked-syscall #'plusp #'fcntl socket f-getfl 0)))
    (handler-bind ()
      ;; FIXME: if this fails, we do not close the socket
      (warn "O_NONBLOCK on the socket ~a did not stick" socket)))
  #+darwin
  (;with-foreign-object (flag :int) ; this gave regularily (but not always) EFAULT
   let ((flag (foreign-alloc :int :initial-contents #(1))))
    (unwind-protect
         (checked-syscall 'zerop 'ioctl socket fionbio flag (foreign-type-size :int))
      (foreign-free flag))
    socket))

(define-condition tcpdelay-setup-failed (communication-error)
  ())

(defun set-nodelay (socket)
  (unless (zerop
           (with-foreign-object (flag :int)
             (setf (mem-ref flag :int) 1)
             (setsockopt socket ipproto-tcp tcp-nodelay
                         flag (foreign-type-size :int))))
    (error 'tcpdelay-setup-failed :medium socket)))

(defmacro with-socket ((socket-name socket) &body body)
  "Run BODY with SOCKET-NAME bound to SOCKET (evaluated). Close the relevant socket
at the end."
  `(let ((,socket-name ,socket))
     (unwind-protect
          (progn
            ,@body)
       (close-fd ,socket-name))))

(defcfun socket :int "man socket(2)" (domain :int) (type :int) (protocol :int))
(defcfun connect :int "man connect(2)" (sockfd :int) (addr :pointer) (addr-len :int))
(defcfun getsockname :int "man getsockname(2)"
  (sockfd :int) (addr :pointer) (addr-len :pointer))
(defcfun (listen-2 "listen") :int "man listen(2)" (sockfd :int) (backlog :int))

(defun call-with-tcp-pair (fn)
  "Apply FN on two interconnected TCP sockets.

This is done by creating server and client sockets and connecting client to the
server.

Makes sure that all created sockets are closed afterwards."
  (flet ((make-socket () (checked-syscall #'plusp #'socket af-inet sock-stream 0)))
    (with-socket (listener (make-socket))
      (socket-bind listener)
      (checked-syscall #'zerop #'listen-2 listener 10)
      (with-socket (client (make-socket))
        (with-foreign-objects ((addr '(:struct sockaddr-in)) (len :int))
          (setf (mem-ref len :int) 4)
          (checked-syscall #'zerop #'getsockname listener addr len)
          (assert (= (mem-ref len :int) size-of-sockaddr-in))
          (checked-syscall #'nonneg-or-eagain #'connect client addr size-of-sockaddr-in))
        (with-socket (server (checked-syscall #'plusp #'accept listener (null-pointer) (null-pointer)))
          (funcall fn server client))))))

(defmacro with-tcp-pair ((a b) &body body)
  "Run BODY with A and B bound to connected sockets.

Close thos sockets afterwards."
  `(call-with-tcp-pair (lambda (,a ,b) ,@body)))

(defun nonneg-or-eagain (res)
  "Helper as first parameter for checked-syscall."
  (or
   (/= res -1)
   (= (errno) EAGAIN)
   (= (errno) EINPROGRESS)))

(define-condition read-error (syscall-error)
  ()
  (:documentation "Syscall error during read-buffer. Should print nicely, e.g.,
```
(http2/tcpip:read-socket* -1)
.. debugger invoked on HTTP2/TCPIP::READ-ERROR:
..   #<READ-ERROR 9 (Bad file descriptor) on -1 during READ>
```"))

(defun write-buffer* (socket vector from to)
  (with-pointer-to-vector-data (buffer vector)
    (let ((res (write-2 socket
                        (inc-pointer buffer from)
                        (- to from)))
          (err (errno)))
      (values res err))))

(defun read-buffer (fd vec &optional (vec-size (length vec)))
  "Read up to VEC-SIZE octets from the FD and store them in the VEC.

Signal DONE or READ-ERROR where there is nothing on the peer.

Return number of read buffer, This means 0 when nothing was read but data may
follow later (EAGAIN)."
  (declare (fixnum fd vec-size)
           (type octet-vector vec))
  (with-pointer-to-vector-data (buffer vec)
    (let ((read (read-2 fd buffer vec-size)))
      (cond ((plusp read) read)
            ((zerop read) (error 'done :medium fd))
            ((> -1 read) (error 'this-cant-happen :medium fd :where 'read-buffer))
            ((/= (errno) EAGAIN)
             (error 'read-error :medium fd :errno (errno) :call 'read))
            (t 0)))))

(defun read-socket* (socket &optional (max-size 4096))
  "This is for debugging/tests only. Return vector read, or nil."
  (let ((vector (http2/utils:make-octet-buffer max-size)))
    (let ((res (read-buffer socket vector max-size)))
      (when (plusp res)
        (values (subseq vector 0 res))))))

(defun fd-to-ip (fd)
  (with-foreign-objects ((addr '(:struct sockaddr-in)) (len :int))
    (setf (mem-ref len :int) size-of-sockaddr-in)
    (checked-syscall #'zerop #'getpeername fd addr len)
    (assert (= (mem-ref len :int) size-of-sockaddr-in))
    (with-foreign-slots ((sin-family sin-port sin-addr) addr (:struct sockaddr-in))
      (values (format nil "~a.~a.~a.~a:~a"
                      (ldb (byte 8 0) sin-addr)
                      (ldb (byte 8 8) sin-addr)
                      (ldb (byte 8 16) sin-addr)
                      (ldb (byte 8 24) sin-addr)
                      (htons sin-port))))))
