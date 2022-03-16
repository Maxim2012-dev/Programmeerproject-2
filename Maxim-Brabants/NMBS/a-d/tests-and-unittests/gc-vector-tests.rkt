#!r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*              Scheme Vector Garbage Collector Tests              *-*-
;-*-*                                                                 *-*-
;-*-*               Theo D'Hondt and Wolfgang De Meuter               *-*-
;-*-*                 2007 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (except (rnrs base) vector make-vector vector-ref vector-set! vector-length vector?)
        (rnrs io simple)
        (rnrs control)
        (a-d scheme-tools)
        (a-d memory automatic vectors vector))
(define (vctr-print v)
  (define l (vector-length v))
  (display v)
  (display " ==> ")
  (display "#( ")
  (do ((idx 0 (+ idx 1)))
    ((= idx l))
    (display (vector-ref v idx))
    (display " "))
  (display ")")
  (newline))
(make-vector 2)
(define r1 (make-vector 2))
(root! r1
       (lambda (r)
         (display "NEW ROOT = ")(display r)(newline)
         ; restore the scheme variables; vectors have moved!
         (set! r1 r)
         (set! r2 (vector-ref r1 0))
         (set! r3 (vector-ref r2 2))))
(make-vector 2)
(define r2 (make-vector 4))
(make-vector 2)
(define r3 (make-vector 4))
(vector-set! r1 0 r2)
(vector-set! r1 1 "1-of-r1")
(vector-set! r2 0 r1)
(vector-set! r2 1 "1-of-r2")
(vector-set! r2 2 r3)
(vector-set! r2 3 r2)
(vector-set! r3 0 r2)
(vector-set! r3 1 "1-of-r3")
(vector-set! r3 2 r1)
(vector-set! r3 3 "3-of-r3")
(vector-ref r1 0)
(vector-ref r1 1)
(vector-ref r2 2)
(define (loopy)
  (do
      ((cntr 0 (+ cntr 1)))
    ((= cntr 100))
    (make-vector (random-inbetween 2 10))))

(define (test)
  (vctr-print r1)
  (vctr-print r2)
  (vctr-print r3)
  (make-vector 10)
  (loopy)
  (vctr-print r1)
  (vctr-print r2)
  (vctr-print r3))
