(defpackage rpg/tests/main
  (:use :cl
        :rpg
        :rove))
(in-package :rpg/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :rpg)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
