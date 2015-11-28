(load "lisp-unit")
(load "polycalc")
(use-package :lisp-unit)

(define-test test-poly+ 
  (assert-equal '((0)) (poly* '((3)) '((3)))))

(run-tests :all)
