#|
2.6
In case representing pairs as procedures wasn’t
mind-boggling enough, consider that, in a language that
can manipulate procedures, we can get by without numbers
(at least insofar as nonnegative integers are concerned)
by implementing 0 and the operation of adding 1 as
|#

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

#|
This representation is known as Church numerals, after its
inventor, Alonzo Church, the logician who invented the λ calculus.
Define one and two directly (not in terms of zero and add1).
|#

(define one
  (lambda (f) (lambda (x) (f x))))
(define two
  (lambda (f) (lambda (x) (f (f x)))))

; define `inc` for testing these
(define (inc n) (+ n 1))

((one inc) 0)
((two inc) 0)

#|
Give a direct definition of the addition procedure +
(not in terms of repeated application of add-1).
|#

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(((add one two) inc) 0)

; test with 3
(define three
  (lambda (f) (lambda (x) (f (f (f x))))))

(((add three two) inc) 0)

