;;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http2/buffer-pool; Base: 10 -*-

(in-package :http2/buffer-pool)

(defsection @buffer-pool (:title "Global Buffer Pool for HTTP/2")
  "Lock-free size-class pooling of octet buffers via CAS (Treiber
stack).  Eliminates allocation in hot paths: frame reading, frame
writing, payload processing.

One global pool shared by all connections.  Buffers grow on demand
and recycle indefinitely.  Max-free cap per size class prevents
unbounded growth after traffic spikes.

Size classes: small (<=16), medium (<=1024), large (<=16384).
Oversized buffers are allocated directly and not pooled."

  (allocate-buffer function)
  (deallocate-buffer function)
  (with-pooled-buffer macro)
  (buffer-pool-stats function)
  (clear-buffer-pool function)
  (@buffer-regions section)
  (@cas section))

(defsection @cas (:title "CAS primitives")
  "Lock-free atomic push/pop on a cons cell head pointer.

Port-specific CAS with generic fallback."
  (atomic-push function)
  (atomic-pop function))

#+sbcl
(defmacro %cas (place old new)
  "Atomic compare-and-swap.  Returns true if swap succeeded."
  `(eq (sb-ext:cas ,place ,old ,new) ,old))

#+lispworks
(defmacro %cas (place old new)
  "Atomic compare-and-swap.  Returns true if swap succeeded."
  `(sys:compare-and-swap ,place ,old ,new))

#-(or sbcl lispworks)
(progn
  (defvar *%cas-lock*
    #+bordeaux-threads (bt:make-lock "cas-fallback")
    #-bordeaux-threads nil
    "Global lock for generic CAS fallback.")

  (defmacro %cas (place old new)
    "Generic CAS fallback.  Correct but serialized."
    (let ((old-val (gensym)) (new-val (gensym)))
      `(let ((,old-val ,old) (,new-val ,new))
         #+bordeaux-threads
         (bt:with-lock-held (*%cas-lock*)
           (cond ((eq ,place ,old-val)
                  (setf ,place ,new-val)
                  t)
                 (t nil)))
         #-bordeaux-threads
         (cond ((eq ,place ,old-val)
                (setf ,place ,new-val)
                t)
               (t nil))))))

(defun atomic-push (value head-cons)
  "Atomically push VALUE onto a list whose head is (CAR HEAD-CONS).
Lock-free via CAS (Treiber stack push)."
  (declare (type cons head-cons))
  (let ((cell (cons value nil)))
    (loop
      (let ((old-head (car head-cons)))
        (setf (cdr cell) old-head)
        (when (%cas (car head-cons) old-head cell)
          (return value))))))

(defun atomic-pop (head-cons)
  "Atomically pop from a list whose head is (CAR HEAD-CONS).
Returns (VALUES element T) on success, (VALUES NIL NIL) if empty.
Lock-free via CAS (Treiber stack pop)."
  (declare (type cons head-cons))
  (loop
    (let ((old-head (car head-cons)))
      (cond ((null old-head)
             (return (values nil nil)))
            ((%cas (car head-cons) old-head (cdr old-head))
             (return (values (car old-head) t)))))))

;;;-------------------------------------------------------------------
;;;
;;; SIZE CLASSES
;;;

(defconstant +small-size+ 16
  "Small buffers: frame headers (9 bytes), small control frames.")

(defconstant +medium-size+ 1024
  "Medium buffers: HPACK encoded headers, small payloads.")

(defconstant +large-size+ 16384
  "Large buffers: DATA/HEADERS payloads up to default max-frame-size.")

(defconstant +max-free-small+ 64
  "Maximum free small buffers retained in pool.")

(defconstant +max-free-medium+ 32
  "Maximum free medium buffers retained in pool.")

(defconstant +max-free-large+ 32
  "Maximum free large buffers retained in pool.")

;;;-------------------------------------------------------------------
;;;
;;; GLOBAL POOL
;;;
;;; Three size classes, each a lock-free Treiber stack.
;;; head-cons is a cons cell whose CAR points to the free list.
;;; free-count is approximate (not atomic) — used only for max-free cap.
;;;

(defstruct (size-class (:conc-name sc-))
  "Lock-free free list of buffers at a fixed size."
  (size 0 :type fixnum :read-only t)
  (max-free 0 :type fixnum :read-only t)
  (head-cons (list nil) :type cons :read-only t)
  (free-count 0 :type fixnum)
  (total-allocated 0 :type fixnum)
  (total-recycled 0 :type fixnum))

(defvar *small-class*
  (make-size-class :size +small-size+ :max-free +max-free-small+))

(defvar *medium-class*
  (make-size-class :size +medium-size+ :max-free +max-free-medium+))

(defvar *large-class*
  (make-size-class :size +large-size+ :max-free +max-free-large+))

(declaim (inline %select-class))

(defun %select-class (size)
  "Select the size class for SIZE bytes, or NIL if oversized."
  (declare (type fixnum size))
  (cond ((<= size +small-size+) *small-class*)
        ((<= size +medium-size+) *medium-class*)
        ((<= size +large-size+) *large-class*)
        (t nil)))

;;;-------------------------------------------------------------------
;;;
;;; ALLOCATE / DEALLOCATE
;;;

(defun allocate-buffer (size)
  "Allocate an octet buffer for at least SIZE bytes with fill-pointer
set to SIZE.  Returns a pooled buffer if available, otherwise allocates
fresh.  The fill-pointer controls (LENGTH buffer) so callers see exactly
the requested size regardless of the underlying size-class capacity.
Lock-free.  Oversized buffers (>16384) are allocated directly."
  (declare (type fixnum size))
  (let ((sc (%select-class size)))
    (cond (sc
           (multiple-value-bind (buf found-p)
               (atomic-pop (sc-head-cons sc))
             (cond (found-p
                    (decf (sc-free-count sc))
                    (incf (sc-total-recycled sc))
                    (setf (fill-pointer buf) size)
                    buf)
                   (t
                    (incf (sc-total-allocated sc))
                    (make-array (sc-size sc)
                                :element-type '(unsigned-byte 8)
                                :fill-pointer size)))))
          (t (make-array size :element-type '(unsigned-byte 8)
                              :fill-pointer size)))))

(defun deallocate-buffer (buffer)
  "Return BUFFER to the global pool.  Lock-free.
Resets fill-pointer to full capacity before pooling.
Oversized or excess buffers are dropped for GC."
  (let* ((size (array-total-size buffer))
         (sc (%select-class size)))
    (when (and sc
               (= size (sc-size sc))
               (< (sc-free-count sc) (sc-max-free sc)))
      (setf (fill-pointer buffer) size)
      (atomic-push buffer (sc-head-cons sc))
      (incf (sc-free-count sc)))
    (values)))

(defmacro with-pooled-buffer ((var size) &body body)
  "Bind VAR to a pooled octet buffer of at least SIZE bytes.
The buffer is returned to the global pool on exit via unwind-protect.
VAR may be larger than SIZE — use :end to delimit the active region."
  (let ((buf-var (gensym "BUF")))
    `(let* ((,buf-var (allocate-buffer ,size))
            (,var ,buf-var))
       (unwind-protect
            (progn ,@body)
         (deallocate-buffer ,buf-var)))))

(defsection @buffer-regions (:title "Buffer regions")
  "Scope-based deallocation for buffers whose lifetime extends beyond
the immediate allocator.  The caller manages buffers locally on the
hot path (zero tracking overhead).  When a buffer escapes local
management (e.g., captured by a continuation closure), the caller
transfers deallocation responsibility to the region via
REGION-TRACK-BUFFER.  On region exit, all tracked buffers are
returned to the pool."
  (with-resource-usage-region macro)
  (region-track-buffer function))

(defvar *current-buffer-region* nil
  "Active buffer region tracking list, or NIL.
Bound per-thread by WITH-RESOURCE-USAGE-REGION.")

(defmacro with-resource-usage-region (() &body body)
  "Establish a buffer region.  On exit (normal or abnormal), deallocate
all buffers that were transferred to the region via REGION-TRACK-BUFFER."
  (let ((head-var (gensym "REGION-HEAD")))
    `(let* ((,head-var (list nil))
            (*current-buffer-region* ,head-var))
       (unwind-protect
            (progn ,@body)
         (%region-cleanup ,head-var)))))

(defun region-track-buffer (buffer)
  "Transfer deallocation responsibility for BUFFER to the active region.
The caller relinquishes ownership; the region will deallocate BUFFER
on exit.  Safe to call when no region is active (no-op)."
  (let ((region *current-buffer-region*))
    (when region
      (push buffer (car region))))
  (values))

(defun %region-cleanup (head)
  "Deallocate all buffers tracked by the region HEAD."
  (declare (type cons head))
  (loop for buf in (car head)
        do (deallocate-buffer buf))
  (setf (car head) nil)
  (values))

;;;-------------------------------------------------------------------
;;;
;;; DIAGNOSTICS
;;;

(defun buffer-pool-stats ()
  "Return a description of global pool utilization."
  (flet ((class-stats (name sc)
           (format nil "~A (~D bytes): ~D allocated, ~D recycled, ~D/~D free"
                   name (sc-size sc)
                   (sc-total-allocated sc)
                   (sc-total-recycled sc)
                   (sc-free-count sc)
                   (sc-max-free sc))))
    (format nil "~A~%~A~%~A"
            (class-stats "Small" *small-class*)
            (class-stats "Medium" *medium-class*)
            (class-stats "Large" *large-class*))))

(defun clear-buffer-pool ()
  "Release all pooled buffers for GC and reset counters."
  (dolist (sc (list *small-class* *medium-class* *large-class*))
    (setf (car (sc-head-cons sc)) nil
          (sc-free-count sc) 0
          (sc-total-allocated sc) 0
          (sc-total-recycled sc) 0))
  (values))
