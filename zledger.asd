;;;; zledger.asd

(asdf:defsystem #:zledger
  :description "TODO"
  :author "jeko <jeko2000@yandex.com"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria
               :log4cl
               :cl-smtp
               :uiop
               :trivial-types
               :hunchentoot
               :cl-ppcre
               :st-json)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "specials")
                 (:file "util")
                 (:file "components")
                 (:file "config")
                 (:file "logger")
                 (:file "http-util")
                 (:file "http")))))
