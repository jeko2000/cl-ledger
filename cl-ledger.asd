;;;; cl-ledger.asd

(asdf:defsystem #:cl-ledger
  :description "TODO"
  :author "jeko <jeko2000@yandex.com"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria
               :log4cl
               :cl-smtp
               :uiop
               :trivial-types)
  :components ((:file "package")
               (:file "specials")
               (:file "components")
               (:file "config")
               (:file "logger")))
