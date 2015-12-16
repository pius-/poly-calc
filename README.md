# poly-calc
Polynomial Calculator in Common Lisp

## Introduction
PolyCalc is a polynomial calculator written in GNU CLISP 2.48. It implements the polynomial arithmetic operations; addition, subtraction and multiplication.

## Instructions
Load polycalc by calling;
```lisp
(load "polycalc")
```
Call the addition, subtraction and multiplication functions;
```lisp
(poly+ p1 p2)
(poly- p1 p2)
(poly* p1 p2)
```
where p1 and p2 are polynomials represented using the representation below.

##Representation
3(x^2)(y^3) + 4x - 1

```lisp
'( (3(x 2)(y 3)) (4(x 1)) (-1) )
```
