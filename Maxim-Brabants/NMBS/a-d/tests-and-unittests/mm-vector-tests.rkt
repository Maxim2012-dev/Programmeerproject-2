#!r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                         Vector MMs Tests                        *-*-
;-*-*                                                                 *-*-
;-*-*                Theo D'Hondt - Wolfgang De Meuter                *-*-
;-*-*              1993-2008 Programming Technology Lab               *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (except (rnrs base) make-vector vector? vector-set! vector-ref vector-length)
        (rnrs io simple)
        (a-d memory manual vectors buddy-system-mm))
        ;(a-d memory manual vectors best-fit-mm))
        ;(a-d memory manual vectors handles-mm))

(define c1 (make-vector 3))
(define c2 (make-vector 25))
(display c1)(newline)
(display c2)(newline)
(vector-set! c1 9 c2)
(vector-set! c2 9 c1)
(vector-set! c1 8 (vector-ref c1 9))
(display (vector-length c1))(newline)
(display (vector-length c2))(newline)
(vector-free c1)
(vector-free c2)
(define stuff (list (make-vector 1) (make-vector 2) (make-vector 3) (make-vector 4) (make-vector 5) (make-vector 6) (make-vector 7) (make-vector 8) (make-vector 9)))(display stuff)
(map vector-free stuff)
