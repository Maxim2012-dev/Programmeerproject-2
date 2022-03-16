#!r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                      Weighted Graphs Test                       *-*-
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
        (a-d graph weighted config))

(require (planet schematics/schemeunit:3/test))

(define directed #t)

(define g (new directed 20))
(add-edge! g 5 10 30)
(check-true (adjacent? g 5 10))
(check-equal? (not directed) (adjacent? g 10 5))
(add-edge! g 10 5 30)
(delete-edge! g 5 10)
(check-false (adjacent? g 5 10)); (not directed))
(check-equal? (adjacent? g 10 5) directed)
(add-edge! g 19 18 50)
(add-edge! g 18 19 40)
(adjacent? g 19 18)
(adjacent? g 18 19)
(weight g 18 19)
(weight g 19 18)

(nr-of-edges g)