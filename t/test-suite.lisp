(defpackage #:zledger/test
  (:use #:cl #:zledger #:fiveam #:alexandria))

(in-package #:zledger/test)

(def-suite zledger-test-suite
  :description "Top-level suite for zledger application.")

(defun run-tests! ()
  (run! 'zledger-test-suite))
