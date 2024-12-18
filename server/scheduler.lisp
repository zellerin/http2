(in-package :http2/server)

(defsection @scheduling
    (:title "Scheduled tasks in server")
  (scheduled-task class)
  (scheduler class)
  (scheduler-in-thread class)
  (schedule-task generic-function)
  (run-scheduler-in-thread function)
  (stop-scheduler-in-thread function)
  (threaded-server-mixin class))

(defclass scheduled-task ()
  ((internal-time-to-run :accessor get-internal-time-to-run :initarg :internal-time-to-run)
   (action-to-run        :accessor get-action-to-run        :initarg :action)
   (action-name          :accessor get-action-name          :initarg :action-name))
  (:default-initargs :action-name nil))

(defclass threaded-server-mixin ()
  ((scheduler :accessor get-scheduler :initarg :scheduler)
   (lock      :accessor get-lock      :initarg :lock))
  (:default-initargs
   :scheduler *scheduler*
   :lock (bt:make-lock))
  (:documentation
   "A mixin for a connection that holds a lock in actions that write to the output network
stream, and provides a second thread for scheduled activities (e.g., periodical
events)."))


(defvar *dummy-last-task*
  (make-instance 'scheduled-task :internal-time-to-run most-positive-fixnum))

(defun time-to-action (task)
  (/ (- (get-internal-time-to-run task)
              (get-internal-real-time))
     1.0
     internal-time-units-per-second))

(defmethod print-object ((task scheduled-task) stream)
  (print-unreadable-object (task stream)
    (if (eq task *dummy-last-task*)
        (format stream "dummy last task")
        (format stream "~a in ~as"
                (or (get-action-name task) (get-action-to-run task))
                (time-to-action task)))))

(defclass scheduler ()
  ((scheduled-tasks :accessor get-scheduled-tasks :initarg :scheduled-tasks)
   (name            :accessor get-name            :initarg :name))
  (:default-initargs
   :scheduled-tasks
   (list *dummy-last-task*)
   :name "A scheduler")
  (:documentation "Simple scheduler of delayed tasks. Holds a list of tasks to run."))

(defmethod schedule-task ((scheduler scheduler) delay action name)
  "Add a new action to a scheduler to be run in DELAY seconds.

Returns SCHEDULER."
  (with-slots (scheduled-tasks) scheduler
    (let* ((new-time-to-run (floor (+ (* internal-time-units-per-second delay)
                                      (get-internal-real-time))))
           (new-task (make-instance 'scheduled-task
                                    :internal-time-to-run new-time-to-run
                                    :action action
                                    :action-name name))
           (next-actions (member new-time-to-run scheduled-tasks
                                 :key #'get-internal-time-to-run
                                 :test #'<)))
      (setf (cdr next-actions) (cons (car next-actions) (cdr next-actions))
            (car next-actions) new-task)))
  scheduler)

(defun get-next-task (scheduler)
  (let ((next-action (car (get-scheduled-tasks scheduler))))
    (values next-action (time-to-action next-action))))

(defun run-mature-tasks (scheduler)
  (loop
    (multiple-value-bind (next time-left) (get-next-task scheduler)
      (when (plusp time-left)
        (return-from run-mature-tasks time-left))
      (pop (get-scheduled-tasks scheduler))
      (funcall (get-action-to-run next))
      (run-mature-tasks scheduler))))

(defmethod print-object ((scheduler scheduler) out)
  (print-unreadable-object (scheduler out :type t)
    (format out "~a task~:p, next ~a" (length (get-scheduled-tasks scheduler))
            (car (get-scheduled-tasks scheduler)))))

(defclass scheduler-in-thread (scheduler)
  ((thread          :accessor get-thread          :initarg :thread))
  (:default-initargs
   :thread nil)
  (:documentation
   "Simple scheduler of delayed tasks that runs in a thread."))

(defmethod schedule-task :after ((scheduler scheduler-in-thread) delay action name)
  "Add a new action to server with scheduler. If scheduler already has a thread,
wake it up by invoking CONTINUE restart."
  (with-slots (thread) scheduler
    (if thread
      (bt:interrupt-thread thread #'continue)
      (run-scheduler-in-thread scheduler))))

(defun run-scheduler-in-thread (scheduler)
  "Make a thread that runs tasks as they mature. CONTINUE restart can be used
during the sleep to re-asses next tasks."
  (setf (get-thread scheduler)
        (bt:make-thread (lambda ()
                          (unwind-protect
                               (loop
                                 (with-simple-restart (continue "Do next action.")
                                   (sleep (run-mature-tasks scheduler))))
                            (setf (get-thread scheduler) nil)))
                        :name (get-name scheduler))))

(defun stop-scheduler-in-thread (scheduler)
  (when (get-thread scheduler)
    (bt:destroy-thread (get-thread scheduler))))

(defvar *scheduler* (make-instance 'scheduler-in-thread))
