(in-package #:zledger)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-declaration*
    '(optimize (speed 0) (safety 3) (debug 3))))

(defvar *logger-daily-file* "zledger.log"
  "The name of the file where logs should be saved.")
(defvar *mailer-host* nil
  "The SMTP host name.")
(defvar *mailer-port* nil
  "The SMTP port name.")
(defvar *mailer-from* nil
  "The from address for outbound emails.")
(defvar *mailer-auth-user-name* nil
  "The SMTP auth login user name.")
(defvar *mailer-auth-password* nil
  "The SMTP auth login password.")
