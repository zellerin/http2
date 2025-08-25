(in-package #:http2/tcpip)
(include "poll.h")
(include "fcntl.h")
(include "sys/socket.h")
(include "netinet/in.h")
(include "netinet/tcp.h")
(include "errno.h")

(cstruct pollfd "struct pollfd"
         (fd "fd" :type :int)
         (events "events" :type :short)
         (revents "revents" :type :short))

(constant (f-setfl "F_SETFL"))
(constant (f-getfl "F_GETFL"))
(constant (o-nonblock "O_NONBLOCK"))
(constant (tcp-nodelay "TCP_NODELAY"))
#+unused (constant (tcp-cork "TCP_CORK"))
(constant (ipproto-tcp "IPPROTO_TCP"))

(constant (c-pollin "POLLIN"))
(constant (c-pollout "POLLOUT"))
(constant (c-pollerr "POLLERR"))
(constant (c-pollhup "POLLHUP"))
(constant (c-pollrdhup "POLLRDHUP") :optional t)
(constant (c-pollnval "POLLNVAL"))

(constant (eagain "EAGAIN"))
(constant (einprogress "EINPROGRESS"))

(constant (pf-inet "PF_INET"))
(constant (af-inet "AF_INET"))
(constant (sock-stream "SOCK_STREAM"))
(cstruct sockaddr-in "struct sockaddr_in"
         (sin-family "sin_family" :type :int16)
         (sin-port "sin_port" :type :uint16)
         (sin-addr "sin_addr" :type :uint32))

(include "sys/ioctl.h")
(constant (fionbio "FIONBIO"))
