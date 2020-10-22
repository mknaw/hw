#|
2.7.
Alyssa’s program is incomplete because she has not specified
the implementation of the interval abstraction.
Here is a definition of the interval constructor:
|#

(define (make-interval a b) (cons a b))

#|
Define selectors upper-bound and lower-bound to
complete the implementation.
|#

(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (car x) (cdr x)))

(define x (make-interval 8 12))
(lower-bound x)
(upper-bound x)

#|
2.8.
Using reasoning analogous to Alyssa’s, describe how the
difference of two intervals may be computed.
Define a corresponding subtraction procedure, called sub-interval.
|#

(define (sub-interval x y)
  (let ((p1 (- (upper-bound x) (lower-bound y)))
        (p2 (- (lower-bound x) (upper-bound y)))
        (p3 (- (lower-bound y) (upper-bound x)))
        (p4 (- (lower-bound y) (upper-bound x))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (lower-bound y) (upper-bound x))))

(define y (make-interval 4 10))
(sub-interval x y)

#|
2.9.
The width of an interval is half of the difference between
its upper and lower bounds. The width is a measure of the
uncertainty of the number specified by the interval. For
some arithmetic operations the width of the result of
combining two intervals is a function only of the widths of
the argument intervals, whereas for others the width of the
combination is not a function of the widths of the argument
intervals. Show that the width of the sum (or difference)
of two intervals is a function only of the widths of the
intervals being added (or subtracted).  Give examples
to show that this is not true for multiplication or division.
|#

(define (width x)
  (* 0.5 (- (upper-bound x) (lower-bound x))))

; if interval w = x + y,
; w = (lower-bound x) + (lower-bound y) .
;     (upper-bound x) + (upper-bound y)
; then width of w can be expressed as
; 0.5 * (   (upper-bound x) + (upper-bound y)
;        - ((lower-bound x) + (lower-bound y))) =
; 0.5 * (   (upper-bound x) - (lower-bound y)
;        +  (upper-bound y) - (lower-bound y)) =
; 0.5 * ((width x) + (width y))
          
(define (sum-width x y)
  (* 0.5 (width x) (width y)))

; if interval w = x - y,
; w = (lower-bound x) - (upper-bound y) .
;     (lower-bound y) - (upper-bound x)
; then width of w can be expressed as
; 0.5 * (   (upper-bound x) + (upper-bound y)
;        - ((lower-bound x) + (lower-bound y))) =
; 0.5 * (   (upper-bound x) - (lower-bound y)
;        +  (upper-bound y) - (lower-bound y)) =
; 0.5 * ((width x) + (width y))


