(in-package #:cl-ledger)

(declaim #.*compile-declaration*)

(alexandria:define-constant +lifecycle-states+ '(:started :starting :stopped)
  :test #'equal
  :documentation "allowed lifecycle states")

(defun lifecycle-state? (state)
  (member state +lifecycle-states+))

(deftype lifecycle-state ()
  `(member ,@+lifecycle-states+))

(defclass lifecycle () ())
(defgeneric start! (lifecycle))
(defgeneric stop! (lifecycle))

;; components
(defvar *components* '())

(define-condition component-missing ()
  ((name
    :type symbol
    :initarg :name
    :reader component-missing-name))
  (:report (lambda (condition out)
             (format out "No such component found: ~s" (component-missing-name condition)))))

(defclass component (lifecycle)
  ((name
    :type symbol
    :initarg :name
    :reader component-name)
   (dependencies
    :type (trivial-types:proper-list component)
    :initarg :dependencies
    :initform nil
    :reader component-dependencies)
   (state
    :type lifecycle-state
    :initform :stopped
    :reader component-state)))

(defun component? (component)
  (typep component 'component))

(defmacro defcomponent (name dependencies &body lifecycle-specs)
  (check-type name symbol)
  (log:info "Attempting to define component ~a" name)
  (let ((sym (gensym "component"))
        start-form
        stop-form)
    (dolist (form lifecycle-specs)
      (ecase (car form)
        (:start (setq start-form (cdr form)))
        (:stop (setq stop-form (cdr form)))))
    `(progn
       (defclass ,name (component) ())
       (defmethod start! ((,sym ,name))
         (declare (ignore ,sym))
         ,@start-form)
       (defmethod stop! ((,sym ,name))
         (declare (ignore ,sym))
         ,@stop-form)
       (push (cons ',name (make-instance ',name
                                         :name ',name
                                         :dependencies ',dependencies))
             *components*)
       t)))

(defun get-component (component-or-name)
  (if (component? component-or-name)
      component-or-name
      (let ((entry (cdr (assoc component-or-name *components*))))
        (unless entry
          (error 'component-missing :name component-or-name))
        entry)))

(defun start-component! (component-or-name)
  (let ((component (get-component component-or-name)))
    (with-slots (name state dependencies) component
      (ecase state
        (:stopped
         (setf state :starting)
         (log:info "Starting component's dependencies ~a" dependencies)
         (dolist (dep dependencies)
           (start-component! dep))
         (log:info "Starting component ~a" name)
         (start! component)
         (setf state :started)
         t)
        (:starting
         (error "Circular component dependency: ~s" name))
        (:started
         (error "Component ~s has already started" name))))))

(defun stop-component! (component-or-name)
  (let ((component (get-component component-or-name)))
    (with-slots (name state dependencies) component
      (ecase state
        (:started
         (log:info "Stopping " dependencies)
         (dolist (dep dependencies)
           (stop-component! dep))
         (log:info "Stopping component ~a" name)
         (stop! component)
         (setf state :stopped)
         t)
        (:starting
         (error "Unable to stop a component just starting: ~s" name))
        (:stopped
         nil)))))

(defun component-started? (component-or-name)
  (let ((component (get-component component-or-name)))
    (eq (component-state component) :started)))
