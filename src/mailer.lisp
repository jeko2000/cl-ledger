;;;; Logger component definition

(in-package #:zledger)
(declaim #.*compile-declaration*)

(defcomponent mailer (logger))

(defclass mail-spec ()
  ((to-name
    :type (or string null)
    :initarg :to-name
    :initform nil
    :reader mail-spec-to-name)
   (to-email
    :type (or string (trivial-types:proper-list string))
    :initarg :to-email
    :reader mail-spec-to-email)
   (subject
    :type string
    :initarg :subject
    :reader mail-spec-subject)
   (content
    :type string
    :initarg :content
    :reader mail-spec-content)))

(defmethod send-email ((mail mail-spec))
  (with-slots (to-name to-email subject content) mail
    (log:info "Sending email to ~s with subject ~a and content length ~d"
              to-email subject (length content))
    (let ((params (list *mailer-host* *mailer-from* to-email subject content
                        :ssl :starttls :port *mailer-port*)))
      (log:info "Attempting to send email with params: ~s" (remove-if #'listp params))
      (apply #'cl-smtp:send-email params))))
