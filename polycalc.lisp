;;;; POLYNOMIAL CALCULATOR
;;;; GNU CLISP 2.48
;;;; 
;;;; Pius Surendralal
;;;; 
;;;; REPRESENTATION
;;;; 3x^2 + 4x - 1 
;;;; '( (3(x 2)) (4(x 1)) (-1) )

;;;; ADDING POLYNOMIALS
;;; poly+: adds two polynomials and returns the result
(defun poly+ (p1 p2)
  (let ((result (sort-terms (append p1 p2))))
    (collect-terms (list(car result)) (cdr result))))

;;; collect: collects like terms
;;; N.B. p1 contains first term of polynomial, p2 contains the remaining terms
(defun collect-terms (p1 p2)
  (cond 
    ((equal 0 (caar p1)) (collect-terms (cdr p1) p2) )
    ((equal 0 (caar p2)) (collect-terms p1 (cdr p2)))
    (t (cond
      ((and (null (car p1)) (null(car p2))) nil) 
      ((null (car p2)) p1)
      ((null (car p1)) p2)
      ; add terms from p2 to p1
      (t (collect-terms (term+poly (car p2) p1) (cdr p2)))))))

;;; term+poly: adds a term to a polynomial and returns the result
(defun term+poly (t1 p1)
  (cond
    ((null p1) (list t1)) 
    ;; if the first term of p1 matches t1 add them together
    ((equal (cdar p1) (cdr t1))
      ;; ignore if sum is zero 
      (if (equal 0 (+ (caar p1) (car t1)))
        (cdr p1)
        (cons (cons (+ (caar p1) (car t1)) (cdr t1)) (cdr p1))))
    ;; if they don't match append the term to the start of the result
    ;; and try to match the next term in the poly
    (t (cons (car p1) (term+poly t1 (cdr p1))))))

;;;; MULTIPLYING POLYNOMIALS
;;; poly* multiplies the first poly by second and returns the result
(defun poly* (p1 p2)
  (let ((result (sort-terms (poly*poly p1 p2))))
    (collect-terms (list(car result))(cdr result))))

;;; poly*: multiplies terms from p1 recursively to all terms in p2
(defun poly*poly (p1 p2)
  (cond
    ((or (null (car p1)) (null (car p2))) nil)
    ;; multiply the first term of p1 by p2
    ;; and repeat for the rest of p1
    (t (append (term*poly (car p1) p2) (poly*poly (cdr p1) p2)))))

;;; term*poly: multiplies a term by a polynomial
(defun term*poly (t1 p1)
  (cond
    ((null p1) nil)
    ;; multiply t1 by the first term of p1
    ;; and repeat for the rest of p1
    (t (cons (term*term t1 (car p1)) (term*poly t1 (cdr p1))))))

;;;; term*term: multiply the first term by the second and return the result
(defun term*term (t1 t2)
  (cond
    ((null t1) t2)
    ;; if they are coefficients, multiply them together 
    ;; and recall the function on the rest of the term
    ((and (atom (car t1))(atom (car t2))) 
     (cons (* (car t1) (car t2)) (term*term (cdr t1) (cdr t2))))
    ;; otherwise multiply t2 by the terms in t1 recursively
    (t (term*term (cdr t1) (var*term (car t1) t2)))))

;;;; var*term: multiply a variable by a term
;;; N.B. coeffs should be dealt with before calling this function
(defun var*term (v1 t1)
  (cond
    ((null t1) (list v1))
    ;; if v1 matches the first variable in t1, add their exponents
    ((equal (caar t1)(car v1))
     (cons (list (car v1) (+ (cadar t1)(cadr v1))) (cdr t1)))
    ;; otherwise append the variable to the start
    ;; and try to match it with the rest of the terms
    (t (cons (car t1) (var*term v1 (cdr t1))))))


;;;; SUBTRACTING POLYNOMIALS
;;; poly-: Subtracts the second polynomial from the first
(defun poly- (p1 p2)
  ;; add first polynomial with the second multiplied by -1
  (poly+ p1 (poly* '((-1)) p2)))


;;;; SORTING
;;; bubble-sort: sorts the variables in the given term
;;; adapted version from: 
;;;   http://www.cs.toronto.edu/~dianaz/Example_LispPart1.html
(defun bubble-sort (t1)
  (cond
    ((null t1) t1)
    ;; if the first term is an atom (coeff) append it to the start
    ;; and sort the rest of the term
    ((atom (car t1)) (cons (car t1) (bubble-sort (cdr t1))))
    ;; if size is 1 return it as already sorted
    ((equal 1 (list-length t1)) t1)
    ;; if in right order, don't swap and carry on sorting
    ((string<= (caar t1) (caadr t1))
     (recheck (cons (car t1) (bubble-sort (cdr t1)))))
    ;; if not in right order, swap the variables and carry on sorting
    (t (recheck (cons (cadr t1) (bubble-sort (cons (car t1) (cddr t1))))))))

;;; recheck: recheck to see if variables are in right order
(defun recheck (t1)
  (if (string<= (caar t1) (caadr t1))
    t1
    (bubble-sort t1)))

;;; sort-terms: sorts the variables in the terms in the poly
(defun sort-terms (p1)
  (map 'list #'bubble-sort p1))
