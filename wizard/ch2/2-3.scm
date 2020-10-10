#|
2.2.
Consider the problem of representing line segments in a plane.
Each segment is represented as a pair of points: a starting
point and an ending point. Define a constructor make-segment
and selectors start-segment and end-segment that define the
representation of segments in terms of points. Furthermore,
a point can be represented as a pair of numbers: the x
coordinate and the y coordinate. Accordingly, specify a
constructor make-point and selectors x-point and y-point that
define this representation.
|#

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment a b) (cons a b))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

#|
Finally, using your selectors and constructors, define a
procedure midpoint-segment that takes a line segment as
argument and returns its midpoint (the point whose coordinates
are the average of the coordinates of the endpoints).
|#

(define (midpoint-segment s)
  (define (mean a b)
    (/ (+ a b) 2))
  (let ((start (start-segment s)) (end (end-segment s)))
    (make-point (mean (x-point start) (x-point end))
                (mean (y-point start) (y-point end)))))

(let ((s (make-segment (make-point 0 0) (make-point 2 4))))
  (print-point (midpoint-segment s)))

#|
2.3.
Implement a representation for rectangles in a plane.
In terms of your constructors and selectors, create procedures
that compute the perimeter and the area of a given rectangle.
|#

; points a & b define the diagonal
(define (make-rectangle a b) (cons a b))
(define (get-width r)
  (abs (- (x-point (car r)) (x-point (cdr r)))))
(define (get-height r)
  (abs (- (y-point (car r)) (y-point (cdr r)))))

(define (perimeter r)
  (* 2 (+ (get-width r) (get-height r))))
(define (area r)
  (* (get-width r) (get-height r)))


(let ((r (make-rectangle (make-point 0 0) (make-point 2 -4))))
  (newline)
  (display (perimeter r))
  (newline)
  (display (area r)))

#|
Now implement a different representation for rectangles.
Can you design your system with suitable abstraction
barriers, so that the same perimeter and area procedures
will work using either representation?
|#

; single origin point a,
; w, h define horizontal and vertical offset from it
(define (make-rectangle a w h) (cons a (cons w h)))
(define (get-width r) (abs (car (cdr r))))
(define (get-height r) (abs (cdr (cdr r))))


(let ((r (make-rectangle (make-point 0 0) -2 4)))
  (newline)
  (display (perimeter r))
  (newline)
  (display (area r)))

