(cl:defpackage #:http2/tests/tcpip
  (:use #:cl #:fiasco #:mgl-pax #:cffi #:http2/tcpip #:http2/utils))

(in-package #:http2/tests/tcpip)

(defsuite
    (http2/tests/tcpip :bind-to-package #:http2/tests/tcpip
                 :in http2/tests::http2/tests))

(deftest errno/test ()
  (is (equal (progn (http2/tcpip::fcntl -1 0 0)
                    (strerror (errno)))
             "Bad file descriptor")))

(deftest strerror/test ()
  "Test that strerror produces a string (reasonable one)"
  (is (equal "Resource temporarily unavailable" (strerror http2/server/poll::eagain))))

(deftest with-tcp-pair/test ()
  "Make a pair of sockets, write to one and read from the other."
  (with-tcp-pair (a b)
    (write-buffer* a (make-initialized-octet-buffer #(1 2 3 4)) 0 4)
    (read-socket* b)))

(deftest set-nonblock/test ()
  "Make a pair of non-blocking sockets, and read from one. It should return nil."
  (with-tcp-pair (a b)
    (set-nonblock a)
    (set-nonblock b)
    (is (null (read-socket* b)))))

(deftest checked-syscall/error ()
  (let ((err (signals http2/tcpip::syscall-error (socket-bind -1))))))
