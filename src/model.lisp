(in-package #:zledger)

(declaim #.*compile-declaration*)

(defclass json-serializable-mixin () ())

(defgeneric as-json (json-serializable))

(defmethod st-json:write-json-element ((serializable json-serializable-mixin) stream)
  (st-json:write-json-element (as-json serializable) stream))

(defclass amount (json-serializable-mixin) ())

(defgeneric %amount+ (amount1 amount2))

(defgeneric %amount= (amount1 amount2))

(defclass null-amount (amount) ())

;; We choose to make null amount  a singleton
(defvar *null-amount* (make-instance 'null-amount))

(defmethod print-object ((amount null-amount) stream)
  (print-unreadable-object (amount stream)
    (format stream "AMOUNT <no amount>")))

(defmethod as-json ((amount null-amount))
  (st-json:jso :null))

(defmethod %amount+ ((amount1 null-amount) (amount2 amount))
  (declare (ignore amount1))
  amount2)

(defmethod %amount+ ((amount1 amount) (amount2 null-amount))
  (declare (ignore amount2))
  amount1)

(defmethod %amount= ((amount1 null-amount) (amount2 amount))
  (eq amount1 *null-amount*))

(defmethod %amount= ((amount1 amount) (amount2 null-amount))
  (eq amount2 *null-amount*))

(defclass simple-amount (amount)
  ((currency :initarg :currency :reader amount-currency)
   (value :initarg :value :reader amount-value)))

(defun make-simple-amount (currency value)
  (make-instance 'simple-amount :currency currency :value value))

(defmethod print-object ((amount simple-amount) stream)
  (print-unreadable-object (amount stream)
    (format stream "AMOUNT currency ~a value ~$"
            (amount-currency amount)
            (amount-value amount))))

(defmethod as-json ((amount simple-amount))
  (with-slots (currency value) amount
    (st-json:jso "currency" currency "value" value)))

(defmethod %amount+ ((amount1 simple-amount) (amount2 simple-amount))
  (unless (string= (amount-currency amount1) (amount-currency amount2))
    (error "Can't sum amounts of different currencies ~s and ~s"
           amount1 amount2))
  (make-simple-amount (amount-currency amount1)
                      (+ (amount-value amount1) (amount-value amount2))))

(defmethod %amount= ((amount1 simple-amount) (amount2 simple-amount))
  (and (string= (amount-currency amount1) (amount-currency amount2))
       (= (amount-value amount1) (amount-value amount2))))

(defun amount+ (&rest amounts)
  (reduce #'%amount+ amounts :initial-value *null-amount*))

(defun amount= (amount &rest more-amounts)
  (loop for amt in more-amounts always (%amount= amount amt)))

(defun make-amount (&optional currency amount)
  (if (and currency amount)
      (make-simple-amount currency amount)
      *null-amount*))

(deftype item-status ()
  `(member :cleared :pending))

(defclass transaction (json-serializable-mixin)
  ((id
    :initarg :id
    :reader transaction-id)
   (entry
    :initarg :entry
    :accessor transaction-entry)
   (account
    :initarg :account
    :accessor transaction-account)
   (amount
    :initarg :amount
    :accessor transaction-amount)
   (note
    :initarg :note
    :accessor transaction-note)))

(defmethod print-object ((tran transaction) stream)
  (print-unreadable-object (tran stream :type t :identity t)
    (with-slots (id account amount) tran
      (format stream ":id ~s :account ~s :amount ~s" id account amount))))

(defun make-transaction (id &key entry account amount note)
  (make-instance 'transaction :id id :entry entry :account account :amount amount :note note))

(defmethod as-json ((tran transaction))
  (with-slots (id entry account amount note) tran
    (st-json:jso "id" id "entry-id" (entry-id entry) "account" account "amount" (as-json amount) "note" note)))

(defclass entry ()
  ((id
    :initarg :id
    :reader entry-id)
   (journal
    :initarg :journal
    :accessor entry-journal)
   (transactions
    :initarg :transactions
    :accessor entry-transactions)
   (status
    :initarg :status
    :accessor entry-status)
   (actual-date
    :initarg :actual-date
    :accessor entry-actual-date)
   (effective-date
    :initarg :effective-date
    :accessor entry-effective-date)
   (note
    :initarg :note
    :accessor entry-note)))

(defmethod print-object ((entry entry) stream)
  (print-unreadable-object (entry stream :type t :identity t)
    (with-slots (id status transactions) entry
      (format stream ":id ~s :status ~s :transaction-count ~d" id status (length transactions)))))

(defun make-entry (id &key journal transactions (status :pending) actual-date effective-date note)
  (make-instance
   'entry
   :id id :journal journal :transactions transactions :status status :actual-date actual-date
   :effective-date effective-date :note note))

(defgeneric add-transaction! (entry transaction)
  (:method ((entry entry) (tran transaction))
    (setf (transaction-entry tran) entry)
    (push tran (entry-transactions entry))))

(defmethod as-json ((entry entry))
  (with-slots (id journal transactions status actual-date effective-date note) entry
    (st-json:jso "id" id "journal" journal "transactions" (mapcar #'as-json transactions)
                 "actual-date" actual-date "effective-date" effective-date "note" note)))
