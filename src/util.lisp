(in-package #:zledger)

(declaim #.*compile-declaration*)

(defun ensure-list (item)
  (if (listp item) item (list item)))

(defun string->keyword (str)
  (intern (string-upcase str) (find-package 'keyword)))
