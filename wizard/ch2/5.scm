#|
2.5.
Show that we can represent pairs of nonnegative integers
using only numbers and arithmetic operations if we represent
the pair a and b as the integer that is the product (2^a)(3^b).
Give the corresponding definitions of the procedures
cons, car, and cdr.
|#
; With 2 and 3 prime, all (2^a)(3^b) have a unique prime
; factorization; we can use it to extract a, b.
; (2^a)(3^b) integers without a factor of 2, 3 correspond
; to a = 0, b = 0 respectively.

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (factor n k)
  (define (iter-factor n k i)
    (if (= (remainder n k) 0)
      (iter-factor (/ n k) k (+ i 1))
      i))
  (iter-factor n k 0))

(define (car n) (factor n 2))
(define (cdr n) (factor n 3))

(let ((n (cons 7 4)))
  (newline)
  (display (car n))
  (newline)
  (display (cdr n)))

(let ((n (cons 0 6)))
  (newline)
  (display (car n))
  (newline)
  (display (cdr n)))
