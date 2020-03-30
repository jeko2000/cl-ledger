(in-package #:zledger)

(declaim #.*compile-declaration*)

(defcomponent app (http))

(defun start-app! () (start-component! 'app))
(defun stop-app! (&optional force) (stop-component! 'app :recursive? t :force force))
