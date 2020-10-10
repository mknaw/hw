
#|
2.4.
Here is an alternative procedural representation of pairs.
|#
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

#|
For this representation, verify that (car (cons x y))
yields x for any objects x and y.
|#

; substitution:
; (car (cons x y)) ->
; ((lambda (m) (m x y)) (lambda (p q) p)) ->
; (((lambda (p q) p) x y)) ->
; (((lambda (p q) p) x y)) ->
; (x y) -> x

#|
What is the corresponding definition of cdr?
|#
(define (cdr z)
  (z (lambda (p q) q)))

(display (cdr (cons 0 1)))

