#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                           Rings Test                            *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (except (rnrs base) length)
        (a-d ring)
        (only (scheme base) require planet))

(require (planet schematics/schemeunit:3/test))

(define r0 (from-scheme-list '()))
(define r1 (from-scheme-list '(1)))
(define r2 (from-scheme-list '(1 2 3 4 5 6 7)))

(check-equal? (length r0) 0)
(check-equal? (length r1) 1)
(check-equal? (length r2) 7)

(check-equal? (peek r2) 7)

(update! r2 10)
(shift-forward! r2)

(check-equal? (peek r2) 1)

(shift-backward! r2)

(check-equal? (peek r2) 10)

(add-before! r2 400)
(shift-forward! r2)
(add-after! r2 500)
(shift-backward! r2)
(shift-backward! r2)
(define one (peek r2))
(shift-forward! r2)
(define two (peek r2))
(shift-forward! r2)
(define three (peek r2))

(check-equal? (+ one two three) 910)

(delete! r2)

(check-equal? (peek r2) 1)

(delete! r2)

(check-equal? (peek r2) 2)

(delete! r2)

(check-equal? (peek r2) 3)

(delete! r2)

(check-equal? (peek r2) 4)

(delete! r2)

(check-equal? (peek r2) 5)

(delete! r2)

(check-equal? (peek r2) 6)

(delete! r2)

(check-equal? (peek r2) 400)

(delete! r2)

(check-equal? (peek r2) 10)

(delete! r2)

(check-equal? (length r2) 0)