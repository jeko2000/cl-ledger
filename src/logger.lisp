;;;; Logger component definition

(in-package #:zledger)

(declaim #.*compile-declaration*)

(defcomponent logger (config)
  (:start
   (when *logger-daily-file*
     (log:config :daily *logger-daily-file*))))
