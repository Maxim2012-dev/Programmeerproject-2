#!r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                     Unweighted Graph Test                       *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (rnrs base)
        (rnrs io simple)
        (only (scheme base) planet require)
        (a-d graph unweighted config))

(require (planet schematics/schemeunit:3/test))

(define directed #f)

(define g (new directed 20))
(add-edge! g 5 10)
(check-true (adjacent? g 5 10))
(check-equal? (adjacent? g 10 5) (not (directed? g)))
(add-edge! g 10 5)
(delete-edge! g 5 10)
(check-false (adjacent? g 5 10))
(check-equal? (adjacent? g 10 5) (directed? g))
(add-edge! g 19 18)
(add-edge! g 18 19)
(check-true (adjacent? g 19 18))
(check-true (adjacent? g 18 19))
(add-edge! g 18 7)
(add-edge! g 5 6)
(add-edge! g 5 8)
(add-edge! g 6 10)
(add-edge! g 18 6)
(add-edge! g 4 18)
(add-edge! g 11 18)
(check-equal? (adjacent? g 7 18) (not (directed? g)))
(check-equal? (nr-of-edges g) (if (directed? g) 10 8))
(check-equal? (order g) 20)
(check-true (unweighted-graph? g))

(define sum 0)
(check-equal? (begin (for-each-node g
                                    (lambda (n)
                                      (set! sum (+ sum n))))
                     sum)
              (let loop
                ((res 0)
                 (n (- (order g) 1)))
                (if (= n 0)
                    res
                    (loop (+ n res) (- n 1)))))

(define copy (make-vector (order g) '()))
(define (fill-test-matrix)
  (let loop
    ((n 0))
    (vector-set! copy n (make-vector (order g) #f))
    (if (< (+ n 1) (order g))
        (loop (+ n 1))))
  (for-each-node
   g
   (lambda (from)
     (for-each-edge
      g
      from
      (lambda (to)
        (vector-set! (vector-ref copy from) to #t))))))

(define (test-test-matrix)
  (let loop1
    ((n 0))
    (let loop2
      ((m 0))
      (if (vector-ref (vector-ref copy n) m)
          (if (not (adjacent? g n m))
              (error "error for" (cons n m))))
      (if (< (+ m 1) (order g))
          (loop2 (+ m 1))))
    (if (< (+ n 1) (order g))
        (loop1 (+ n 1))
        (display "seems ok"))))
(fill-test-matrix)
(test-test-matrix)

(delete-edge! g 18 7)
(delete-edge! g 5 6)
(delete-edge! g 18 6)

(fill-test-matrix)
(test-test-matrix)