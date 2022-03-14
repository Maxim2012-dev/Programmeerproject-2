#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                      Scheme-- Pair Memory                       *-*-
;-*-*                                                                 *-*-
;-*-*                 Theo D'Hondt - Wolfgang De Meuter               *-*-
;-*-*               1993-2009 Programming Technology Lab              *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (pair)
 (export cons free car cdr set-car! set-cdr! pair? null)
 (import (rename (rnrs base) (cons scheme:cons) (pair? scheme:pair?) (car scheme:car) (cdr scheme:cdr))
         (rnrs control))
 
 (define null ())
 
 (define pair-tag 'pair)
 (define memory-size 5)
 
 (define car-memory
   (do ((memory (make-vector memory-size null))
        (addr 1 (+ addr 1)))
     ((= addr memory-size ) memory)
     (vector-set! memory (- addr 1) addr)))
 
 (define cdr-memory
   (make-vector memory-size null))
 
 (define next-free 0)
  
 (define (make addr)
   (scheme:cons pair-tag addr))
 
 (define (address pair)
   (scheme:cdr pair))
 
 (define (cons car cdr)
   (define addr next-free)
   (if (eq? addr null)
     (error "storage overflow" cons))
   (set! next-free (vector-ref car-memory addr))
   (vector-set! car-memory addr car)
   (vector-set! cdr-memory addr cdr)
   (make addr))
 
 (define (pair? any)
   (and (scheme:pair? any)
        (eq? (scheme:car any) pair-tag)))
 
 (define (car pair)
   (vector-ref car-memory (address pair)))
 
 (define (cdr pair)
   (vector-ref cdr-memory (address pair)))
 
 (define (set-car! pair any)
   (vector-set! car-memory (address pair) any))
 
 (define (set-cdr! pair any)
   (vector-set! cdr-memory (address pair) any))
 
 (define (free pair)
   (define addr (address pair))
   (vector-set! car-memory addr next-free)
   (set! next-free addr)))