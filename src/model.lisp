(in-package #:zledger)

(declaim #.*compile-declaration*)

(deftype item-status ()
  `(member :cleared :pending))

(defclass transaction ()
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

(defmethod st-json:write-json-element ((tran transaction) stream)
  (with-slots (id entry account amount note) tran
    (st-json:jso "id" id "entry-id" (entry-id entry) "account" account "amount" amount "note" note)))

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

(defmethod st-json:write-json-element ((entry entry) stream)
  (with-slots (id journal transactions status actual-date effective-date note) entry
    (st-json:jso "id" id "journal" journal "transactions" (mapcar (alexandria:rcurry #'st-json:write-json-element stream) transactions)
                 "actual-date" actual-date "effective-date" effective-date
                 "note" note)))
