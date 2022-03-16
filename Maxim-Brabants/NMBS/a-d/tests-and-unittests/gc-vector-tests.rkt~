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
        (a-d memory automatic vectors vector))

(make-vector 2)
(define r1 (make-vector 1))
(make-vector 2)
(define r2 (make-vector 3))
(make-vector 2)
(define r3 (make-vector 2))
(vector-set! r1 0 r2)
(vector-set! r2 0 r1)
(vector-set! r2 1 r3)
(vector-set! r2 2 r2)
(vector-set! r3 0 r2)
(vector-set! r3 1 r1)
(root! r1) 
(make-vector 8)
;(display memory)
