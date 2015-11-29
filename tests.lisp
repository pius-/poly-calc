(load "lisp-unit")
(load "polycalc")
(use-package :lisp-unit)

(setq *print-failures* t)

(define-test test-poly+ 
  (assert-equal '(()) (poly+ '(()) '(())))
)

(run-tests :all)

