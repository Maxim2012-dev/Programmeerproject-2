#!r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*            Tests for Scheme Pair Garbage Collectors             *-*-
;-*-*                                                                 *-*-
;-*-*               Theo D'Hondt and Wolfgang De Meuter               *-*-
;-*-*                 2007 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (except (rnrs base) cons car cdr pair?)
        (a-d memory automatic pairs pair))

(define c1 (cons 1 2))
(define c2 (cons 3 4))
(define c3 (cons c1 c2))
(set-car! c1 (cdr c3))
(set-cdr! c2 (car c3))
(root! c3)
(let loop ((i 1))
  (cons i (+ i 1))
  (if (< i 20)
    (loop (+ i 1))))
(cons 2 2)
(define c4 (cons 5 6))
(set-car! c1 c4)
(set-car! c2 c4)
(let loop ((i 1))
  (cons i (+ i 1))
  (if (< i 40)
    (loop (+ i 1))))
(cons 2 2)
