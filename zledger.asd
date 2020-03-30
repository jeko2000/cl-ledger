;;;; zledger.asd

(defsystem "zledger"
  :license  "MIT"
  :description "Web-based personal accounting application"
  :author "jeko2000 (Johnny Ruiz)"
  :maintainer "jeko2000 (Johnny Ruiz)"
  :mailto "jeko2000@yandex.com"
  :source-control (:git "git@github.com:jeko2000/zledger.git")
  :version (:read-file-form "version.lisp-expr")
  :encoding :utf-8
  :serial t
  :depends-on ("alexandria"
               "log4cl"
               "cl-smtp"
               "uiop"
               "trivial-types"
               "hunchentoot"
               "cl-ppcre"
               "st-json"
               "cl-arrows")
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "specials" :depends-on ("package"))
                 (:file "util" :depends-on ("specials"))
                 (:file "http-util" :depends-on ("util"))
                 (:file "component" :depends-on ("util"))
                 (:file "config" :depends-on ("component"))
                 (:file "logger" :depends-on ("component"))
                 (:file "mailer" :depends-on ("component"))
                 (:file "http" :depends-on ("component" "http-util"))
                 (:file "model" :depends-on ("component"))
                 (:file "app" :depends-on ("model" "http")))))
  :in-order-to ((test-op (test-op "zledger/test"))))

(defsystem "zledger/test"
  :depends-on ("zledger" "fiveam")
  :components ((:module "t"
                :components
                ((:file "test-suite")
                 (:file "util-test" :depends-on ("test-suite")))))
  :perform (test-op (o c) (symbol-call :zledger/test '#:run-tests!)))
