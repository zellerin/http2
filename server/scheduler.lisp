(in-package :http2)


(defclass scheduled-task ()
  ((internal-time-to-run :accessor get-internal-time-to-run :initarg :internal-time-to-run)
   (action-to-run          :accessor get-action-to-run          :initarg :action)))

(defun time-to-action (task)
  (ceiling (- (get-internal-time-to-run task)
            (get-internal-real-time))
         (floor internal-time-units-per-second 1000)))

(defmethod print-object ((task scheduled-task) stream)
  (print-unreadable-object (task stream)
    (format stream "in ~dms" (time-to-action task))))



(defclass scheduler ()
  ((scheduled-tasks :accessor get-scheduled-tasks :initarg :scheduled-tasks)
   (name            :accessor get-name            :initarg :name)
   (thread          :accessor get-thread          :initarg :thread))
  (:default-initargs
   :scheduled-tasks
   (list
    (make-instance 'scheduled-task :internal-time-to-run most-positive-fixnum))
   :name "A scheduler"
   :thread nil)
  (:documentation
   "Simple scheduler of delayed tasks."))

(defun schedule-task (scheduler delay action)
  "Add a new action to server with scheduler. If scheduler already has a thread,
wake it up."
  (with-slots (scheduled-tasks thread) scheduler
    (let* ((new-time-to-run (+ delay (get-internal-real-time)))
           (new-task (make-instance 'scheduled-task
                                    :internal-time-to-run new-time-to-run
                                    :action action))
           (next-actions (member new-time-to-run scheduled-tasks
                                   :key #'get-internal-time-to-run
                                   :test #'<)))
      (setf (cdr next-actions) (cons (car next-actions) (cdr next-actions))
            (car next-actions) new-task))
    (when thread
      (bt:interrupt-thread thread #'continue))))

(defun run-scheduler-in-thread (scheduler)
  "Make a thread that runs tasks as they mature. CONTINUE restart can be used
during the sleep to re-asses next tasks."
  (setf (get-thread scheduler)
        (bt:make-thread (lambda ()
                          (loop
                            for next-action = (car (get-scheduled-tasks scheduler))
                            for time-to-next = (time-to-action next-action)
                            if (plusp time-to-next)
                              do
                                 (with-simple-restart (continue "Do next action.")
                                     (sleep (/ time-to-next 1000))
                                   (continue))
                            else
                              do
                                 (funcall (get-action-to-run next-action))
                                 (pop (get-scheduled-tasks scheduler))))
                        :name (get-name scheduler))))

(defmethod initialize-instance :after ((s scheduler) &key &allow-other-keys)
  (run-scheduler-in-thread s))

(defgeneric cleanup-connection (connection)
  (:method (connection) nil)
  (:method :after ((connection threaded-server-mixin))
    (ignore-errors ; we might be in shutdown phase and thread gone
     (bt:destroy-thread (get-thread (get-scheduler connection)))))
  (:documentation
   "Remove resources associated with a server connection. Called after connection is
closed."))
