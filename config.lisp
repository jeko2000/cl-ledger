;;;; Configuration component definition

(in-package #:cl-ledger)

(declaim #.*compile-declaration*)

(defcomponent config ())

(load "cl-ledger.config")
