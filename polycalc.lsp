; add two polynomials
; if first is null return second
; if second is null retrun first
; otherwise recursively add terms from second poly to first
(defun poly+ (p1 p2)
  (cond 
    ((null p1) p2)
    ((null p2) p1)
    (t (poly+ (poly+term p1 (car p2))  (cdr p2) ))) )

;add the term to the polynomial
;if polynomial is empty return the term
;if the terms match add them
;otherwise repeat for rest of polynoaial
(defun poly+term (p1 t1)
  (cond
    ((null p1) t1)
    ((equal (cdar p1) (cdr t1)) (term+term (car p1) t1))
    (t (list (car p1) (poly+term (cdr p1) t1)))
))

;add two terms
;assuming they are same add coefficients
(defun term+term (t1 t2)
  (cons (+ (car t1) (car t2)) (cdr t1)))

(print (poly+ '((3 (a 2)(b 2))) '((2 (a 2)))))
