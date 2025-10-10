;;;; Copyright 2022-2025 by Tomáš Zellerin

(in-package :http2/utils)

(defsection @utils (:title "Utilities")
  (find-setting-code function)
  (find-setting-by-id function)

  (stream-id type)
  (http2-stream-state type)

  (make-octet-buffer function)
  (make-initialized-octet-buffer function)
  (aref/wide function)
  (vector-from-hex-text function)
  (frame-size type)
  (octet-vector type)

  (trace-object macro)
  (*example-url* variable))

(export '&)

(declaim (inline make-octet-buffer))

#|
|#

(deftype frame-size ()
  "The size of a frame payload is limited by the maximum size that a
receiver advertises in the SETTINGS_MAX_FRAME_SIZE setting.  This
setting can have any value between 2^14 (16,384) and 2^24-1
(16,777,215) octets, inclusive."
  '(unsigned-byte 24))

(defun make-octet-buffer (size)
  "
```cl-transcript
(make-octet-buffer 10)
=> #(0 0 0 0 0 0 0 0 0 0)

(type-of *)
=> (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (10))
```"
  (declare (type frame-size size))
  (make-array size :element-type '(unsigned-byte 8)))

(defun make-initialized-octet-buffer (content)
  (make-array (length content) :element-type '(unsigned-byte 8)
              :initial-contents content))



(deftype octet-vector (&optional length)
  "An octet vector of length LENGTH (if specified)"
  `(array (unsigned-byte 8) (,length)))

(defun aref/wide (sequence start size)
  "Construct a little indian number from SIZE octets in SEQUENCE, starting at START.

```cl-transcript
=> ``CL-TRANSCRIPT
(aref/wide (make-initialized-octet-buffer #(1 2 3)) 0 0)
=> 0

(aref/wide (make-initialized-octet-buffer #(1 2 3)) 0 2)
=> 258

(aref/wide  (make-initialized-octet-buffer #(1 2 3)) 0 3)
=> 66051

(aref/wide  (make-initialized-octet-buffer #(1 2 3)) 0 4)
.. debugger invoked on SB-INT:INVALID-ARRAY-INDEX-ERROR:
..   Invalid index 3 for (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (3)), should be a non-negative integer below 3.
=> ; No value

```
"
  (declare ((integer 0 8) size))
  (loop with res = 0
        for i from 0 to (+ -1 size)
        do
           (setf (ldb (byte 8 (* 8 (- size 1 i))) res)
                 (aref sequence (+ start i)))
        finally (return res)))

(defun (setf aref/wide) (value sequence start size)
  "Store a little indian number as SIZE octets to the SEQUENCE.

```cl-transcript
(let ((seq (make-octet-buffer 5)))
  (setf (aref/wide seq 0 3) 66051)
  seq)
=> #(1 2 3 0 0)
```
"
  (declare ((integer 1 8) size))
  (loop for i from 0 to (+ -1 size)
        do
           (setf
            (aref sequence (+ start i))
            (ldb (byte 8 (* 8 (- size 1 i))) value))
        finally (return value)))

(defun vector-from-hex-text (text)
  "Convert a hex string to an octet vector."
  (declare (string text))
  (loop with prefix = text
        for i from 0 to (1- (length prefix)) by 2
        collect (parse-integer prefix :start i :end (+ i 2) :radix 16) into l
        finally (return (map '(vector (unsigned-byte 8)) 'identity l))))


;;;; Settings
(defvar *settings*
  #(nil
    (:HEADER-TABLE-SIZE 1  "Allows the sender to inform the
      remote endpoint of the maximum size of the header compression
      table used to decode header blocks, in octets.  The encoder can
      select any size equal to or less than this value by using
      signaling specific to the header compression format inside a
      header block (see [COMPRESSION]).  The initial value is 4,096
      octets.")

    (:ENABLE-PUSH 2 "This setting can be used to disable
      server push (Section 8.2).  An endpoint MUST NOT send a
      PUSH-PROMISE frame if it receives this parameter set to a value of
      0.  An endpoint that has both set this parameter to 0 and had it
      acknowledged MUST treat the receipt of a PUSH-PROMISE frame as a
      connection error (Section 5.4.1) of type PROTOCOL-ERROR.

      The initial value is 1, which indicates that server push is
      permitted.  Any value other than 0 or 1 MUST be treated as a
      connection error (Section 5.4.1) of type PROTOCOL-ERROR.")

    (:MAX-CONCURRENT-STREAMS 3  "Indicates the maximum number
      of concurrent streams that the sender will allow.  This limit is
      directional: it applies to the number of streams that the sender
      permits the receiver to create.  Initially, there is no limit to
      this value.  It is recommended that this value be no smaller than
      100, so as to not unnecessarily limit parallelism.

      A value of 0 for SETTINGS-MAX-CONCURRENT-STREAMS SHOULD NOT be
      treated as special by endpoints.  A zero value does prevent the
      creation of new streams; however, this can also happen for any
      limit that is exhausted with active streams.  Servers SHOULD only
      set a zero value for short durations; if a server does not wish to
      accept requests, closing the connection is more appropriate.")

    (:INITIAL-WINDOW-SIZE 4  "Indicates the sender's initial
      window size (in octets) for stream-level flow control.  The
      initial value is 2^16-1 (65,535) octets.

      This setting affects the window size of all streams (see
      Section 6.9.2).

      Values above the maximum flow-control window size of 2^31-1 MUST
      be treated as a connection error (Section 5.4.1) of type
      FLOW-CONTROL-ERROR.")

    (:MAX-FRAME-SIZE 5  "Indicates the size of the largest
      frame payload that the sender is willing to receive, in octets.

      The initial value is 2^14 (16,384) octets.  The value advertised
      by an endpoint MUST be between this initial value and the maximum
      allowed frame size (2^24-1 or 16,777,215 octets), inclusive.
      Values outside this range MUST be treated as a connection error
      (Section 5.4.1) of type PROTOCOL-ERROR.")

    (:MAX-HEADER-LIST-SIZE 6  "This advisory setting informs a
      peer of the maximum size of header list that the sender is
      prepared to accept, in octets.  The value is based on the
      uncompressed size of header fields, including the length of the
      name and value in octets plus an overhead of 32 octets for each
      header field.

      For any given request, a lower limit than what is advertised MAY
      be enforced.  The initial value of this setting is unlimited.")

    (:accept-cache-digest 7             ; not part of standard
     "A server can notify its support for CACHE_DIGEST frame by sending the
      SETTINGS_ACCEPT_CACHE_DIGEST (0x7) SETTINGS parameter. If the server is
      tempted to making optimizations based on CACHE_DIGEST frames, it SHOULD
      send the SETTINGS parameter immediately after the connection is
      established.

      The value of the parameter is a bit-field of which the following bits are
      defined:

      ACCEPT (0x1): When set, it indicates that the server is willing to make
      use of a digest of cached responses.

      Rest of the bits MUST be ignored and MUST be left unset when sending.

      The initial value of the parameter is zero (0x0) meaning that the server
      is not interested in seeing a CACHE_DIGEST frame.

      Some underlying transports allow the server's first flight of application
      data to reach the client at around the same time when the client sends
      it's first flight data. When such transport (e.g., TLS 1.3
      [I-D.ietf-tls-tls13] in full-handshake mode) is used, a client can
      postpone sending the CACHE_DIGEST frame until it receives a
      SETTINGS_ACCEPT_CACHE_DIGEST settings value.

      When the underlying transport does not have such property (e.g., TLS 1.3
      in 0-RTT mode), a client can reuse the settings value found in previous
      connections to that origin [RFC6454] to make assumptions.")

    (:enable-connect-protocol 8
     "See RFC8441. Upon receipt of SETTINGS_ENABLE_CONNECT_PROTOCOL with a value of
   1, a client MAY use the Extended CONNECT as defined in this document when
   creating new streams.  Receipt of this parameter by a server does not have
   any impact.")
    (:no-rfc5740-priorities 9 "See RFC9218.")
    (:tls-reneg-permitted #x10 "MS-HTTP2E"))
  "See https://www.iana.org/assignments/http2-parameters/http2-parameters.xhtml")

(defun find-setting-code (name)
  "Find setting name by code

```cl-transcript
(find-setting-code :max-header-list-size)
=> 6

(find-setting-code :foo)
.. debugger invoked on SIMPLE-ERROR:
..   No setting named FOO
=> ; No value
```
"
  (or (position name *settings* :key #'car)
      (error "No setting named ~a" name)))

(defun find-setting-by-id (id)
  (if (< 0 id (length *settings*))
      (car (aref *settings* id))
      (format nil "ID#~d" id)))

(deftype stream-id ()
  "Streams are identified with an unsigned 31-bit  integer."
  `(unsigned-byte 31))

(deftype http2-stream-state ()
  "HTTP2 state. Currently a symbol from fixed list, might be a number in future."
  '(member idle open closed
    half-closed/local half-closed/remote
    reserved/local reserved/remote))


(defvar *trace-level* 1)
(defvar *global-trace-level* 1)

(defvar *example-url* "https://example.com")

(defmacro trace-object (function level
                        (format &rest args)
                        &optional after-format)
  "Add reporting tracing for FUNCTION. See sbcl manual for trace to see what a
function can be.

The FORMAT is a format string and ARGS is order of an argument in the function input.

This is intended primarily for SBCL. It would use basic tracing on other platform (and this is not tested...)"
  `(handler-bind
       ((simple-warning #'muffle-warning))
     (trace ,function .
            #-sbcl nil
            #+sbcl (:report nil
                    :condition (> ,(or (find-symbol "*TRACE-LEVEL*" *package*)
                                       '*global-trace-level*)
                                  ,level)
                    ,@(when format `(:print
                                     (flet ((& (nr) (sb-debug:arg nr)))
                                       (format nil "(>~d) ~?" ,level ,format (list ,@args)))))
                    ,@(when after-format
                        (destructuring-bind (format-after &rest args-after) after-format
                          `(:print-after
                            (flet ((& (nr) (sb-debug:arg nr)))
                              (format nil "(<~d) ~? " ,level ,format-after (list ,@args-after))))))))))

(defsection @cerpaths ()
  (h2-server-context-mixin class)
  (certificated-context-mixin class)
  (easy-certificated-context-mixin class)
  (certificate-file slot)
  (private-key-file slot))

(defclass certificated-context-mixin ()
  ((certificate-file :initarg  :certificate-file)
   (private-key-file :initarg  :private-key-file))
  (:documentation
   "Dispatcher with two slots, CERTIFICATE-FILE and PRIVATE-KEY-FILE, that are used
for TLS context creation."))

(defclass easy-certificated-context-mixin (certificated-context-mixin)
  ()
  (:default-initargs :hostname "localhost")
  (:documentation "Uses HOSTNAME (defaulting to localhost) to locate or create the key pair."))

(defun find-private-key-file (hostname)
  "Find the private key for HOSTNAME or create it.

Look for
- /etc/letsencrypt/live/<hostname>privkey.pem (this is where let's encrypt stores them)
- file named <hostname>.key in /tmp (ad-hoc generated files)

If it does not exist, generate the key and self signed cert in /tmp/"
  (let* ((key-name (make-pathname :name hostname :defaults "/tmp/foo.key"))
         (cert-name (make-pathname :type "crt" :defaults key-name))
         (lets-encrypt-name
           (make-pathname :directory `(:absolute "etc" "letsencrypt" "live" ,hostname)
                          :name "privkey"
                          :type "pem")))
    (cond ((probe-file key-name))
          ((probe-file lets-encrypt-name) lets-encrypt-name) ; explicit needed, symlinks
          (t
           (warn "No private key found by heuristics, creating new pair in /tmp")
           (maybe-create-certificate key-name cert-name :base "/tmp")))))

(defun find-certificate-file (keypath)
  "Find a certificate file for private key stored in KEYPATH.

Try file of same name ending with .crt, or, if the name of private key was privkey.pem, try fullchain.pem (this is what let's encrypt uses)."
  (or
   (probe-file (make-pathname :type "crt" :defaults keypath))
   (and (equal (pathname-name keypath) "privkey")
        (probe-file (make-pathname :name "fullchain" :defaults keypath)))
   (error "Cannot find cert file")))

(defun maybe-create-certificate (key certificate &key system (base
                                                              (if system (asdf:component-pathname (asdf:find-system system)) #P"/tmp/")))
  "Generate key and a self-signed certificate to it for localhost using openssl
cli."
  (unless (and (probe-file key)
               (probe-file certificate))
    (let ((key-file (ensure-directories-exist (merge-pathnames key base)))
          (cert-file (ensure-directories-exist (merge-pathnames certificate base))))
      (uiop:run-program
       `("openssl" "req" "-new" "-nodes" "-x509" "-days" "365" "-subj" "/CN=localhost" "-keyout" ,(namestring key-file)
                   "-outform" "PEM" "-out" ,(namestring cert-file)))
      (terpri)
      (values key-file cert-file))))

(defmethod initialize-instance :after ((object easy-certificated-context-mixin) &key hostname)
  (when hostname
    (with-slots (private-key-file certificate-file) object
      (setf private-key-file (namestring (find-private-key-file hostname))
            certificate-file (namestring (find-certificate-file private-key-file))))))

(defsection @errs ()
  "There is quite a few error conditions there, but some packages need common
ancestors to descend from. "
  (communication-error condition)
  (done condition)
  (get-medium generic-function)
  (get-medium (method (communication-error))))

(define-condition communication-error (serious-condition)
  ((medium :accessor get-medium :initarg :medium))
  (:documentation "Something happens that prevents communication from going on, unless handled."))

(define-condition done (communication-error)
  ()
  (:documentation "This condition is signalled when the socket on the other side is closed (for
reading)."))

(defmethod print-object ((err communication-error) out)
  (print-unreadable-object (err out :type t)
    (format out "on ~a" (http2/utils:get-medium err))))
