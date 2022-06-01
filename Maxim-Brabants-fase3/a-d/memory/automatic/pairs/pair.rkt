#lang r6rs
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                   Scheme Pair Memory Manager                    *-*-
;-*-*         (with iterative mark&sweep garbage collection)          *-*-
;-*-*               Theo D'Hondt and Wolfgang De Meuter               *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
(library
 (pair)
 (export root! car cdr set-car! set-cdr! cons null pair?)
 (import (rename (rnrs base) (car scheme:car) (cdr scheme:cdr)
                 (cons scheme:cons) (pair? scheme:pair?))
         (rnrs io simple)
         ;(a-d memory automatic pairs stop-and-copy))
         ;(a-d memory automatic pairs recursive))
        (a-d memory automatic pairs tail-recursive))
 
 (define (pair? exp)
   (and (scheme:pair? exp)
        (eq? (scheme:car exp) pair-tag)))
 
 (define (car pair)
   (car-peek (untag pair)))
 
 (define (cdr pair)
   (cdr-peek (untag pair)))
 
 (define (set-car! pair car)
   (car-poke! (untag pair) car))
 
 (define (set-cdr! pair cdr)
   (cdr-poke! (untag pair) cdr))
 
 (define (cons car cdr)
   (if (out-of-memory?)
     (gc))
   (if (out-of-memory?)
     (error "storage overflow" cons))
   (let ((addr (allocate)))
     (car-poke! addr car)
     (cdr-poke! addr cdr)
     (tag addr))))