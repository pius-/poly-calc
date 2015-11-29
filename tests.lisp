(load "lisp-unit")
(load "polycalc")
(use-package :lisp-unit)

(setq *print-failures* t)
(setq *print-errors* t)

(define-test test-poly+ 
  (assert-equal '() (poly+ () ()))
  (assert-equal '((4)) (poly+ '((4)) ()))
  (assert-equal '((4)) (poly+ () '((4))))
  (assert-equal '() (poly+ '((0)) '((0))))
  (assert-equal '((4)) (poly+ '((4)) '((0))))
  (assert-equal '((4)) (poly+ '((0)) '((4))))
  (assert-equal '() (poly+ '((3)) '((-3))))
  (assert-equal '((7)) (poly+ '((4)) '((3))))
  (assert-equal '((1)) (poly+ '((4)) '((-3))))
  (assert-equal '((7.5)) (poly+ '((4.5)) '((3))))

  (assert-equal '((2(x 1))) (poly+ '((2(x 1))) ()))
  (assert-equal '((2(x 1))) (poly+ () '((2(x 1)))))
  (assert-equal '() (poly+ '((0(x 1))) '((0(x 1)))))
  (assert-equal '((4(x 2))) (poly+ '((4(x 2))) '((0(x 2)))))
  (assert-equal '((4(x 2))) (poly+ '((0(x 2))) '((4(x 2)))))
  (assert-equal '() (poly+ '((3(x 2))) '((-3(x 2)))))
  (assert-equal '((7(x 3))) (poly+ '((4(x 3))) '((3(x 3)))))
  (assert-equal '((1(x 3))) (poly+ '((4(x 3))) '((-3(x 3)))))
  (assert-equal '((7.5(x 4))) (poly+ '((4.5(x 4))) '((3(x 4)))))

  (assert-equal '((3(x 1))) (poly+ '((1(x 1)) (1(x 1))) '((1(x 1)))))
  (assert-equal '((3(x 1))) (poly+ '((1(x 1))) '((1(x 1)) (1(x 1)))))
  (assert-equal '((1(x 1)) (1(y 1))) (poly+ '((1(x 1))) '((1(y 1)))))
  (assert-equal '((2(x 1)) (2(y 1))) (poly+ '((1(x 1)) (1(y 1))) '((1(y 1)) (1(x 1)))))
  (assert-equal '((2(x 1)) (1(y 1)) (1(z 1))) (poly+ '((1(x 1)) (1(y 1))) '((1(x 1)) (1(z 1)))))

  (assert-equal '((4(x 1)(z 3)) (5) (2(y 2))) (poly+ '((2(x 1)(z 3)) (3)) '((2(x 1)(z 3)) (2(y 2)) (2))))
  
  (assert-equal '((1(x 1)(y 2)) (2(x 1))) (poly+ '((1(x 1)(y 2)) (1(x 1))) '((1(x 1)))))
  (assert-equal '((1(x 1))) (poly+ '((1(x 1)(y 2))) '((-1(y 2)(x 1)) (1(x 1)))))
  (assert-equal '((2(x 1)(y 1))) (poly+ '((1(x 1)(y 1))) '((1(y 1)(x 1))))))


(define-test test-poly* 
  (assert-equal '() (poly* () ()))
  (assert-equal '() (poly* '((4)) ()))
  (assert-equal '() (poly* () '((4))))
  (assert-equal '() (poly* '((0)) '((0))))
  (assert-equal '() (poly* '((4)) '((0))))
  (assert-equal '() (poly* '((0)) '((4))))
  (assert-equal '((-9)) (poly* '((3)) '((-3))))
  (assert-equal '((12)) (poly* '((4)) '((3))))
  (assert-equal '((-12)) (poly* '((4)) '((-3))))
  (assert-equal '((13.5)) (poly* '((4.5)) '((3))))

  (assert-equal '() (poly* '((2(x 1))) ()))
  (assert-equal '() (poly* () '((2(x 1)))))
  (assert-equal '() (poly* '((0(x 1))) '((0(x 1)))))
  (assert-equal '() (poly* '((4(x 2))) '((0(x 2)))))
  (assert-equal '() (poly* '((0(x 2))) '((4(x 2)))))
  (assert-equal '((-9(x 4))) (poly* '((3(x 2))) '((-3(x 2)))))
  (assert-equal '((12(x 6))) (poly* '((4(x 3))) '((3(x 3)))))
  (assert-equal '((-12(x 6))) (poly* '((4(x 3))) '((-3(x 3)))))
  (assert-equal '((13.5(x 8))) (poly* '((4.5(x 4))) '((3(x 4)))))

  (assert-equal '((2(x 2))) (poly* '((1(x 1)) (1(x 1))) '((1(x 1)))))
  (assert-equal '((2(x 2))) (poly* '((1(x 1))) '((1(x 1)) (1(x 1)))))
  (assert-equal '((1(x 1)(y 1))) (poly* '((1(x 1))) '((1(y 1)))))
  (assert-equal '((2(x 1)(y 1)) (1(x 2)) (1(y 2))) (poly* '((1(x 1)) (1(y 1))) '((1(y 1)) (1(x 1)))))
  (assert-equal '((1(x 2)) (1(x 1)(z 1)) (1(x 1)(y 1)) (1(y 1)(z 1))) (poly* '((1(x 1)) (1(y 1))) '((1(x 1)) (1(z 1)))))

  (assert-equal '((4(x 2)(z 6)) (4(x 1)(y 2)(z 3)) (10(x 1)(z 3)) (6(y 2)) (6)) (poly* '((2(x 1)(z 3)) (3)) '((2(x 1)(z 3)) (2(y 2)) (2))))
  
  (assert-equal '((1(x 2)(y 2)) (1(x 2))) (poly* '((1(x 1)(y 2)) (1(x 1))) '((1(x 1)))))
  (assert-equal '((-1(x 2)(y 4)) (1(x 2)(y 2))) (poly* '((1(x 1)(y 2))) '((-1(y 2)(x 1)) (1(x 1)))))
  (assert-equal '((1(x 2)(y 2))) (poly* '((1(x 1)(y 1))) '((1(y 1)(x 1))))))


(define-test test-poly- 
  (assert-equal '() (poly- () ()))
  (assert-equal '((4)) (poly- '((4)) ()))
  (assert-equal '((-4)) (poly- () '((4))))
  (assert-equal '() (poly- '((0)) '((0))))
  (assert-equal '((4)) (poly- '((4)) '((0))))
  (assert-equal '((-4)) (poly- '((0)) '((4))))
  (assert-equal '((6)) (poly- '((3)) '((-3))))
  (assert-equal '((1)) (poly- '((4)) '((3))))
  (assert-equal '((7)) (poly- '((4)) '((-3))))
  (assert-equal '((1.5)) (poly- '((4.5)) '((3))))
  
  (assert-equal '((2(x 1))) (poly- '((2(x 1))) ()))
  (assert-equal '((-2(x 1))) (poly- () '((2(x 1)))))
  (assert-equal '() (poly- '((0(x 1))) '((0(x 1)))))
  (assert-equal '((4(x 2))) (poly- '((4(x 2))) '((0(x 2)))))
  (assert-equal '((-4(x 2))) (poly- '((0(x 2))) '((4(x 2)))))
  (assert-equal '((6(x 2))) (poly- '((3(x 2))) '((-3(x 2))))) 
  (assert-equal '((1(x 3))) (poly- '((4(x 3))) '((3(x 3)))))
  (assert-equal '((7(x 3))) (poly- '((4(x 3))) '((-3(x 3)))))
  (assert-equal '((1.5(x 4))) (poly- '((4.5(x 4))) '((3(x 4)))))
  
  (assert-equal '((1(x 1))) (poly- '((1(x 1)) (1(x 1))) '((1(x 1)))))
  (assert-equal '((-1(x 1))) (poly- '((1(x 1))) '((1(x 1)) (1(x 1)))))
  (assert-equal '((1(x 1)) (-1(y 1))) (poly- '((1(x 1))) '((1(y 1)))))
  (assert-equal '() (poly- '((1(x 1)) (1(y 1))) '((1(y 1)) (1(x 1)))))
  (assert-equal '((1(y 1)) (-1(z 1))) (poly- '((1(x 1)) (1(y 1))) '((1(x 1)) (1(z 1)))))
  
  (assert-equal '((1) (-2(y 2))) (poly- '((2(x 1)(z 3)) (3)) '((2(x 1)(z 3)) (2(y 2)) (2))))
  
  (assert-equal '((1(x 1)(y 2))) (poly- '((1(x 1)(y 2)) (1(x 1))) '((1(x 1)))))
  (assert-equal '((2(x 1)(y 2)) (-1(x 1))) (poly- '((1(x 1)(y 2))) '((-1(y 2)(x 1)) (1(x 1)))))
  (assert-equal '() (poly- '((1(x 1)(y 1))) '((1(y 1)(x 1))))))

;(run-tests '(test-poly+))
;(run-tests '(test-poly*))
;(run-tests '(test-poly-))

(run-tests :all)
