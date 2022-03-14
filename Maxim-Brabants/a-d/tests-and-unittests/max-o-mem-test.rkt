#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                         max-o-mem test                          *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universitent Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (rnrs base)
        (rnrs mutable-pairs)
        (rnrs io simple)
        (prefix (a-d examples max-o-mem) mom:)
        (prefix (a-d examples complex (1)) complex:))

(define (greatest lst << init)
  (define max (mom:new << init))
  (define (iter lst)
    (mom:write! max (car lst))
    (if (not (null? (cdr lst)))
      (iter (cdr lst))))
  (iter lst)
  (mom:read max))

; Exampe with complex numbers
(define (complex< c1 c2)
  (define (square x) (* x x))
  (< (sqrt (+ (square (complex:real c1))
              (square (complex:imag c1))))
     (sqrt (+ (square (complex:real c2))
              (square (complex:imag c2))))))
              
(define complex-mom (mom:new complex< (complex:new 0 0)))
(define number-mom (mom:new < 0))

(define integer-list (list 1 2 3 4 5))
(define complex-list (list (complex:new 1 0) (complex:new 0 1) (complex:new 3 4) (complex:new 4 3)))

(display (greatest integer-list < 0))(newline)
(display (greatest complex-list complex< (complex:new 0 0)))(newline)