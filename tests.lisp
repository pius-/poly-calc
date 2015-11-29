(load "lisp-unit")
(load "polycalc")
(use-package :lisp-unit)

(setq *print-failures* t)
(setq *print-errors* t)

(define-test test-poly+ 
  (assert-equal '(()) (poly+ '(()) '(())))
  (assert-equal '((4)) (poly+ '((4)) '(())))
  (assert-equal '((4)) (poly+ '(()) '((4))))
  (assert-equal '((7)) (poly+ '((4)) '((3))))
)


(run-tests :all)
