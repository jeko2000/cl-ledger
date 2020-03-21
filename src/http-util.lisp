(in-package #:zledger)

(declaim #.*compile-declaration*)

(defclass route ()
  ((path
    :initarg :path
    :reader route-path)
   (method
    :initarg :method
    :reader route-method)
   (groups
    :initarg :groups
    :reader route-groups)
   (path-scanner
    :initarg :path-scanner
    :reader route-path-scanner)
   (exact-path?
    :initarg :exact-path?
    :reader route-exact-path?)
   (handler
    :initarg :handler
    :reader route-handler))
  (:default-initargs
   :path nil
   :method nil))

(let ((token-regex (cl-ppcre:create-scanner ":([A-Za-z][A-Za-z0-9]*)"))
      (replacement "([^/]+)"))
  (defun get-parameter-keywords (path)
    (when path
      (let (groups)
        (cl-ppcre:do-register-groups (group) (token-regex path)
          (push (string->keyword group) groups))
        (values (nreverse groups)
                (cl-ppcre:regex-replace-all token-regex path replacement))))))

(defun make-route (handler &key method path)
  (multiple-value-bind (groups path-scanner) (get-parameter-keywords path)
    (make-instance 'route
                   :handler handler
                   :method method
                   :path path
                   :groups groups
                   :path-scanner path-scanner
                   :exact-path? (null groups))))

(defmethod route-match?! ((route route) request)
  (with-slots (path-scanner groups method exact-path handler) route
    (and (or (null path-scanner)
             (multiple-value-bind (match regs)
                 (cl-ppcre:scan-to-strings
                  path-scanner (hunchentoot:request-uri* request))
               (when match
                 (loop for group in groups
                       for reg across regs
                       do (setf (hunchentoot:aux-request-value group) reg)
                       finally (return t)))))
         (or (null method)
             (member (hunchentoot:request-method* request)
                     (ensure-list method))))))

(defmethod print-object ((route route) stream)
  (print-unreadable-object (route stream :type t :identity t)
    (format stream "path ~s method ~a"
            (route-path route)
            (route-method route))))

(defvar *routes* nil)

(defclass http-server (hunchentoot:acceptor)
  ()
  (:default-initargs
   :name "HTTP Server"
   :address "127.0.0.1"))

(defmethod hunchentoot:acceptor-dispatch-request ((server http-server) request)
  (loop for route in *routes*
        when (route-match?! route request)
          return (funcall (route-handler route))
        finally (call-next-method)))

(defun make-http-server (&key (host "127.0.0.1") (port 8080))
  (make-instance 'http-server :address host :port port))

(defmacro defhandler (name (&rest specs &key method path &allow-other-keys) &body body)
  (declare (ignore method path))
  `(progn
     (defun ,name ()
       ,@body)
     (setf *routes*
           (delete-if (lambda (route)
                        (eq ',name (route-handler route)))
                      *routes*))
     (push (make-route ',name ,@specs)
           *routes*)))

(defmacro with-json-response ((&optional (status 200)) &body body)
  (let ((json (gensym "json")))
    `(progn
       (setf (hunchentoot:content-type*) "application/json"
             (hunchentoot:return-code*) ,status)
       (let ((,json (st-json:write-json-to-string (progn ,@body))))
         (log:info "[HTTP ~d] Body: ~a" ,status ,json)
         ,json))))

(defmacro with-path-parameters (params &body body)
  `(let ,(loop for param in params
               collect `(,param (hunchentoot:aux-request-value ,(string->keyword param))))
     ,@body))
