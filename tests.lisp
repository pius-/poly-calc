(load "lisp-unit")
(load "polycalc")
(use-package :lisp-unit)

(setq *print-failures* t)
(setq *print-errors* t)

(define-test test-poly+ 
  (assert-equal '(()) (poly+ '(()) '(())))
  (assert-equal '((4)) (poly+ '((4)) '(())))
  (assert-equal '((4)) (poly+ '(()) '((4))))
  (assert-equal '((0)) (poly+ '((0)) '((0))))
  (assert-equal '((0)) (poly+ '((3)) '((-3))))
  (assert-equal '((7)) (poly+ '((4)) '((3))))
  (assert-equal '((1)) (poly+ '((4)) '((-3))))
  (assert-equal '((7.5)) (poly+ '((4.5)) '((3))))

  (assert-equal '((1(x 1)) (1(y 1))) (poly+ '((1(x 1))) '((1(y 1)))))
  (assert-equal '((3(x 1))) (poly+ '((1(x 1)) (1(x 1))) '((1(x 1)))))
  (assert-equal '((2(x 1)) (2(y 1))) (poly+ '((1(x 1)) (1(y 1))) '((1(y 1)) (1(x 1)))))
  (assert-equal '((2(x 1)) (1(y 1)) (1(z 1))) (poly+ '((1(x 1)) (1(y 1))) '((1(x 1)) (1(z 1)))))
  (assert-equal '((2(x 1))) (poly+ '((1(x 1)) (1(x 1))) '((0))))
 
  (assert-equal '((4(x 2)) (3)) (poly+ '((4(x 2))) '((3))))
  (assert-equal '((3) (4(x 2))) (poly+ '((3)) '((4(x 2)))))
  (assert-equal '((4(x 2)(y 3)) (3)) (poly+ '((4(x 2)(y 3))) '((3))))
  (assert-equal '((4(x 2)) (3(y 3))) (poly+ '((2(x 2))) '((2(x 2)) (3(y 3)))))
  (assert-equal '((2(x 1)) (2(y 1))) (poly+ '((1(x 1)) (1(y 1))) '((1(x 1)) (1(y 1)))))
)


(run-tests :all)
