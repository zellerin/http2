(in-package #:http2)

(defsection  @buffer-stream-and-pipes
    (:title "Vector backed streams and (buffered) octet pipes")

  "In tests we need to send data between client and server as if using a
stream. For this, we implement pipes that allow to send data to one stream and
receive them from another (MAKE-PIPE), and possibly also vice
versa (MAKE-FULL-PIPE).

The pipe streams are backed by vectors - that is, one vector shared by both pipe
ends.

This should probably be some existing library, but I failed to find it.

At the moment, it is strictly for the purpose of testing, so not generally usable:

- no synchronization, usage in concurrently running processes not expected,
- performance satisfying but not optimized"
  (make-pipe function)
  (make-full-pipe function)
  (pipe-end-for-read class)
  (pipe-end-for-write class)
  (@buffer-stream-and-pipes-impl section))

(mgl-pax:defsection @buffer-stream-and-pipes-impl
    (:title "Vector backed streams - impementation"
     :export nil)
  (trivial-gray-streams:stream-read-byte (method () (pipe-end-for-read)))
  (trivial-gray-streams:stream-listen (method () (pipe-end-for-read)))
  (trivial-gray-streams:stream-write-byte (method () (pipe-end-for-write t))))

(defclass pipe-end-for-read (binary-stream trivial-gray-streams:fundamental-binary-input-stream)
  ((buffer :accessor get-buffer :initarg :buffer)
   (index  :accessor get-index  :initarg :index)
   (end    :accessor get-end    :initarg :end
           :initform nil))
  (:documentation "Octet stream backed by a buffer.

Each instance has a buffer and an index to the first unread element of it. It is
assumed that whole the buffer can be read."))

(defclass pipe-end-for-write (binary-stream trivial-gray-streams:fundamental-binary-output-stream)
  ((write-buffer :accessor get-write-buffer :initarg :write-buffer)))

(defun make-pipe (&key (buffer-size 4096))
  "Two values, each representing one end of a freshly created one-way binary
pipe: writer and reader. They share the buffer."
  (let ((buffer (make-array buffer-size :adjustable t
                                        :fill-pointer 0)))
    (values (make-instance 'pipe-end-for-write
                           :write-buffer buffer)
            (make-instance 'pipe-end-for-read :buffer buffer
                                              :index 0))))

(defmethod print-object ((pipe pipe-end-for-read) stream)
  (print-unreadable-object (pipe stream :type t)))

(defmethod print-object ((pipe pipe-end-for-write) stream)
  (print-unreadable-object (pipe stream :type t)))

(defun make-full-pipe ()
  "Two values, each representing one end of a full binary pipe: writes to ones are
read from the other."
  (multiple-value-bind (write-stream-a read-stream-a) (make-pipe)
    (multiple-value-bind (write-stream-b read-stream-b) (make-pipe)
      (values
       (make-two-way-stream read-stream-b write-stream-a)
       (make-two-way-stream read-stream-a write-stream-b)))))

(defmethod trivial-gray-streams:stream-read-byte ((stream pipe-end-for-read))
  (if (= (get-index stream) (length (get-buffer stream))) :eof
      (prog1 (aref (get-buffer stream) (get-index stream))
        (incf (get-index stream)))))

(defmethod trivial-gray-streams:stream-write-byte ((stream pipe-end-for-write) byte)
  (vector-push-extend byte (get-write-buffer stream)))

(defmethod trivial-gray-streams:stream-listen ((stream pipe-end-for-read))
  "If the index is not on end of stream, it can probably be read.

Note that this does not work on clisp, see
https://clisp.sourceforge.io/impnotes/non-block-io.html"
  (< (get-index stream) (length (get-buffer stream))))
