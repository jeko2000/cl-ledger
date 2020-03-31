(in-package #:zledger)

(declaim #.*compile-declaration*)

(defun make-compound-symbol (&rest symbols)
  (intern (format nil "~{~a~}" symbols)))

(defmacro with-postfixed-slots (postfix slots instance &body body)
  (progn
    `(with-slots ,(loop for slot in slots collect `(,(make-compound-symbol slot postfix) ,slot)) ,instance
       ,@body)))
