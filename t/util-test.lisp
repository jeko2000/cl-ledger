(in-package #:zledger/test)

(in-suite zledger-test-suite)

(test util-tests
  (is (eq :sample (string->keyword "sample"))))
