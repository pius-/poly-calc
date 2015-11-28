;;;; ADDING POLYNOMIALS
;;; poly+: adds two polynomials and returns the result
;;; p1: the first polynomial
;;; p2: the second polynomial
(defun poly+ (p1 p2)
  ;; let p1 be the fist term in p1
  ;; and p2 be p2 appended to the rest of p1
  (let ((p1 (list (car p1)))
        (p2 (append (cdr p1) p2)))
    (collect p1 p2)))

;;; collect: adds terms from p2 to p1
;;; p1: the first polyomial, initially only containing one term
;;; p2: the second polynomial, initially containing the rest of the terms
(defun collect (p1 p2)
  (cond 
    ;; base case if p2 is null return p1
    ((null p2) p1)
    ;; add the first term of p2 to p1 
    ;; and call collect on the result and the rest of p2
    (t (collect (term+poly (car p2) p1) (cdr p2)))))

;;; term+poly: adds a term to a polynomial and returns the result
;;; t1: the term
;;; p1: the polynomial
(defun term+poly (t1 p1)
  (cond
    ;; base case if p1 is empty return t1
    ((null p1) (list t1))
    ;; if the first term of p1 matches t1 add them together
    ((equal (cdar p1) (cdr t1)) (cons (term+term (car p1) t1) (cdr p1)))
    ;; if they don't match append the term to the start of the result
    ;; and try to match the next term in p1
    (t (cons (car p1) (term+poly t1 (cdr p1))))))

;;; term+term: adds two terms and returns the result
;;; t1: the first term
;;; t2: the second term
(defun term+term (t1 t2)
  ;; assuming the terms have the same variables
  ;; add the coefficients together and append the variable to the end
  (cons (+ (car t1) (car t2)) (cdr t1)))


;;;; MULTIPLYING POLYNOMIALS
;;; poly*: multiplies first polynomial to second and returns the result
;;; p1: the first polynomial
;;; p2: the second polynomial
(defun poly* (p1 p2)
  (cond
    ;; base case if p1 is null return nil
    ((null p1) nil)
    ;; otherwise multiply the first term of p1 by p2
    ;; and append the result to poly* of the rest of p1 and p2
    (t (append (term*poly (car p1) p2) (poly* (cdr p1) p2)))))

;;; term*poly: multiplies a term by a polynomial
;;; t1: the term
;;; p1: the polynomial
(defun term*poly (t1 p1)
  (cond
    ;; base case if p1 is null return nil
    ((null p1) nil)
    ;; otherwise multiply t1 by the first term of p1
    ;; and append it to the term multiplied by the rest of the polynomial
    (t (cons (term*term t1 (car p1)) (term*poly t1 (cdr p1))))))

;;;; term*term: multiply the first term by the second and return the result
;;; t1: the first term
;;; t2: the second term
(defun term*term (t1 t2)
  (cond
    ;; base case if t1 is null return t2
    ((null t1) t2)
    ;; if coefficients, multiply them together 
    ;; and recall the function on the rest of the term
    ((and (atom (car t1))(atom (car t2))) 
     (cons (* (car t1) (car t2)) (term*term (cdr t1) (cdr t2))))
    ;; otherwise multiply t2 by the terms in t1 recursively
    (t (term*term (cdr t1) (var*term (car t1) t2)))))

;;;; var*term: multiply a variable by a term
;;; v1: the variable
;;; t1: the term
;;; N.B. coeffs should be dealt with before calling this function
(defun var*term (v1 t1)
  (cond
    ;; base case if t1 is null return v1
    ((null t1) (list v1))
    ;; if v1 matches the first variable in t1
    ;; add their exponents
    ((equal (caar t1)(car v1))
     (cons (list (car v1) (+ (cadar t1)(cadr v1))) (cdr t1)))
    ;; otherwise append the variable to the start
    ;; and try to match it with next var in the term
    (t (cons (car t1) (var*term v1 (cdr t1))))))


;;;; SUBTRACTING POLYNOMIALS
;;; poly-: Subtracts the second polynomial from the first
;;; p1: the first polynomial
;;; p2: the second polynomial
(defun poly- (p1 p2)
  ;; add first polynomial with the second multiplied by -1
  (poly+ p1 (poly* '((-1)) p2)))

(print 
  ;(poly+ '( (2) ) '( (1) ))
  ;(poly+ '( (2(x 1)) ) '( (2(y 2)) ))
  ;(poly+ '( (2(x 1)) ) '( (2(x 1)) ))
  (poly+ '( (1(x 1)) (1(y 1)) ) '( (1(x 1)) (1(y 1)) ))
  ;(poly+ '( (2(x 1)(y 1)) (3(x 1)) ) '( (1(x 1)) (2(z 3)) (3(x 1)(y 1)) ))

  ;(poly* '( (2) ) '( (4) ))
  ;(poly* '( (2(x 1)) ) '( (2(y 2)) ))
  ;(poly* '( (2(x 1)) ) '( (2(x 1)) ))
  ;(poly* '( (1(x 1)) (1(y 1)) ) '( (1(x 1)) (1(y 1)) ))
  ;(poly* '( (2(x 1)(y 1)) (3(x 1)) ) '( (1(x 1)) (2(z 3)) (3(x 1)(y 1)) ))

  ;(poly- '( (2) ) '( (1) ))
  ;(poly- '( (2(x 1)) ) '( (2(y 2)) ))
  ;(poly- '( (2(x 1)) ) '( (2(x 1)) ))
  ;(poly- '( (1(x 1)) (1(y 1)) ) '( (1(x 1)) (1(y 1)) ))
  ;(poly- '( (2(x 1)(y 1)) (3(x 1)) ) '( (1(x 1)) (2(z 3)) (3(x 1)(y 1)) ))
)

