(in-package #:zledger)

(declaim #.*compile-declaration*)

(defclass equality-mixin () ())
(defgeneric equal? (object1 object2)
  (:method (object1 object2)
    nil))

(defclass json-serializable-mixin () ())
(defgeneric as-json (json-serializable))
(defmethod st-json:write-json-element ((serializable json-serializable-mixin) stream)
  (st-json:write-json-element (as-json serializable) stream))

(defclass amount (json-serializable-mixin equality-mixin) ())
(defgeneric %amount+ (amount1 amount2))
(defgeneric %amount= (amount1 amount2))

(defclass null-amount (amount) ())

;; We choose to make null amount a singleton
(defvar *null-amount* (make-instance 'null-amount))

(defmethod print-object ((amount null-amount) stream)
  (print-unreadable-object (amount stream)
    (format stream "AMOUNT <no amount>")))

(defmethod as-json ((amount null-amount))
  :null)

(defmethod equal? ((amount1 null-amount) (amount2 null-amount))
  (and (eql *null-amount* amount1) (eql *null-amount* amount2)))

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
  ((currency
    :initarg :currency
    :reader amount-currency
    :type string)
   (value
    :initarg :value
    :reader amount-value
    :type real)))

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

(defmethod equal? ((amount1 simple-amount) (amount2 simple-amount))
  (with-postfixed-slots 1 (currency value) amount1
    (with-postfixed-slots 2 (currency value) amount2
      (and (string= currency1 currency2)
           (= value1 value2)))))

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

(defclass transaction (json-serializable-mixin equality-mixin)
  ((id
    :initarg :id
    :reader transaction-id
    :type string)
   (entry-id
    :initarg :entry-id
    :accessor transaction-entry-id
    :type (or string null))
   (account
    :initarg :account
    :accessor transaction-account
    :type (or string null))
   (amount
    :initarg :amount
    :accessor transaction-amount
    :type amount)
   (note
    :initarg :note
    :accessor transaction-note
    :type (or string null))))

(defmethod print-object ((tran transaction) stream)
  (print-unreadable-object (tran stream :type t :identity t)
    (with-slots (id account amount) tran
      (format stream ":id ~s :account ~s :amount ~s" id account amount))))

(defun make-transaction (id account &key amount entry-id note)
  (make-instance 'transaction :id id :account account :entry-id entry-id
                              :amount (or amount *null-amount*) :note note))

(defmethod as-json ((tran transaction))
  (with-slots (id entry-id account amount note) tran
    (st-json:jso "id" id "entryId" entry-id "account" account "amount" (as-json amount) "note" note)))

(defmethod equal? ((tran1 transaction) (tran2 transaction))
  (with-postfixed-slots 1 (id entry-id account amount note) tran1
    (with-postfixed-slots 2 (id entry-id account amount note) tran2
      (and (string= id1 id2)
           (string= entry-id1 entry-id2)
           (string= account1 account2)
           (equal? amount1 amount2)
           (string= note1 note2)))))

(defclass entry (json-serializable-mixin equality-mixin)
  ((id
    :initarg :id
    :reader entry-id)
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

(defun make-entry (id &key transactions (status :pending) actual-date effective-date note)
  (make-instance
   'entry
   :id id :transactions transactions :status status :actual-date actual-date
   :effective-date effective-date :note note))

(defgeneric add-transaction! (entry transaction)
  (:method ((entry entry) (tran transaction))
    (setf (transaction-entry-id tran) (entry-id entry))
    (push tran (entry-transactions entry))))

(defmethod as-json ((entry entry))
  (with-slots (id transactions status actual-date effective-date note) entry
    (st-json:jso "id" id "transactions" (mapcar #'as-json transactions)
                 "actual-date" actual-date "effective-date" effective-date "note" note)))

(defgeneric from-json (json class))

(defmethod from-json (json (class (eql 'amount)))
  (if (eql json :null)
      *null-amount*
      (make-amount (st-json:getjso "currency" json)
                   (st-json:getjso "value" json))))

(defmethod from-json (json (class (eql 'transaction)))
  (make-transaction (st-json:getjso "id" json)
                    (st-json:getjso "account" json)
                    :entry-id (st-json:getjso "entryId" json)
                    :amount (from-json (st-json:getjso "amount" json) 'amount)
                    :note (st-json:getjso "note" json)))
