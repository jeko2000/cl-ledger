(in-package #:zledger)

(declaim #.*compile-declaration*)

(define-constant +lifecycle-states+ '(:started :starting :stopped)
  :test #'equal
  :documentation "Allowed lifecycle states")

(defun lifecycle-state? (state)
  (member state +lifecycle-states+))

(deftype lifecycle-state ()
  `(member ,@+lifecycle-states+))

(defclass lifecycle () ())
(defgeneric start! (lifecycle))
(defgeneric stop! (lifecycle))

;; components
(defvar *components* '())

(define-condition component-missing-error (error)
  ((name
    :type symbol
    :initarg :name
    :reader component-missing-name))
  (:report (lambda (condition out)
             (format out "No such component found: ~a" (component-missing-name condition)))))

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

(defmethod print-object ((component component) stream)
  (print-unreadable-object (component stream :type t)
    (with-slots (name state dependencies) component
      (format stream "name ~a :state ~a :dependencies ~a"
              name state (or dependencies "<none>")))))

(defun component? (component)
  (typep component 'component))

(defmacro defcomponent (name dependencies &body lifecycle-specs)
  (check-type name symbol)
  (let ((sym (gensym "component"))
        start-form
        stop-form)
    (dolist (form lifecycle-specs)
      (ecase (car form)
        (:start (setq start-form (cdr form)))
        (:stop (setq stop-form (cdr form)))))
    `(progn
       ;; TODO: Should we dispatch by symbol instead?
       (defclass ,name (component) ())
       (defmethod start! ((,sym ,name))
         (declare (ignore ,sym))
         ,@start-form)
       (defmethod stop! ((,sym ,name))
         (declare (ignore ,sym))
         ,@stop-form)
       (deletef *components* ',name :key #'component-name)
       (push (make-instance ',name
                            :name ',name
                            :dependencies ',dependencies)
             *components*)
       t)))

(defun get-component (component-or-name)
  (if (component? component-or-name)
      component-or-name
      (let ((entry (find component-or-name *components* :key #'component-name)))
        (unless entry
          (error 'component-missing-error :name component-or-name))
        entry)))

(defun start-component! (component-or-name)
  (let ((component (get-component component-or-name)))
    (with-slots (name state dependencies) component
      (ecase state
        (:stopped
         (setf state :starting)
         (when dependencies
           (log:info "Starting component's dependencies ~a" dependencies)
           (dolist (dep dependencies)
             (start-component! dep)))
         (log:info "Starting component ~a" name)
         (start! component)
         (setf state :started)
         t)
        (:starting
         (error "Circular component dependency: ~s" name))
        (:started
         t)))))

(defun stop-component! (component-or-name &key force recursive?)
  (flet ((%stop-component (component force recursive?)
           (with-slots (name state dependencies) component
             (when recursive?
               (log:info "Recursively stopping dependencies ~a" dependencies)
               (dolist (dep dependencies)
                 (stop-component! dep :force force :recursive? recursive?)))
             (log:info "Stopping component ~a" name)
             (stop! component)
             (setf state :stopped))
           t))
    (let ((component (get-component component-or-name)))
      (ecase (component-state component)
        (:started
         (%stop-component component force recursive?))
        (:starting
         (if force
             (%stop-component component force recursive?)
             (error "Unable to stop a component just starting: ~s" (component-name component))))
        (:stopped
         nil)))))

(defun component-started? (component-or-name)
  (eql :started
       (-> component-or-name
           (get-component)
           (component-state))))
