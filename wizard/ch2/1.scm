#|
Define a better version of make-rat that handles both 
positive and negative arguments. make-rat should normalize
the sign so that if the rational number is positive,
both the numerator and denominator are positive, and if
the rational number is negative, only the numerator is negative.
|#

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (print-rat x)
  (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))

(print-rat (make-rat 6 9))
(print-rat (make-rat 6 -9))
(print-rat (make-rat -6 9))
(print-rat (make-rat -6 -9))

