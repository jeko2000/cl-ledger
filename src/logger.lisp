;;;; Logger component definition

(in-package #:zledger)

(declaim #.*compile-declaration*)

(defcomponent logger ()
  (:start
   (when *logger-daily-file*
     (log:config :daily *daily-logger-file*))))
