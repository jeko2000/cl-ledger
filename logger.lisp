;;;; Logger component definition

(in-package #:cl-ledger)
(declaim #.*compile-declaration*)

(defcomponent logger ()
  (:start
   (when **logger-daily-file*
     (log:config :daily *daily-logger-file*))))
