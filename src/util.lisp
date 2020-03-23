(in-package #:zledger)

(declaim #.*compile-declaration*)

(defun string->keyword (str)
  (intern (string-upcase str) (find-package 'keyword)))
