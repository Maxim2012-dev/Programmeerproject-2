#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                   Buddy System Memory Manager                   *-*-
;-*-*                                                                 *-*-
;-*-*                Theo D'Hondt - Wolfgang De Meuter                *-*-
;-*-*              1993-2009 Programming Technology Lab               *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (buddy-system-mm)
 (export make-vector vector-free vector? vector-ref vector-set! vector-length)
 (import (except (rnrs base) set! vector? vector-ref vector-set! vector-length make-vector)
        ;(a-d memory manual vectors exponential (1)))
        (a-d memory manual vectors exponential (2)))
         ;(a-d memory manual vectors fibonacci))
 
 (define vector-tag 'vector)
 
 (define (address+index addr index)
   (if (number? index)
     (if (negative? index)
       (error "index must be non-negative" index)
       (let
           ((size (- (size-free addr) overhead)))
         (if (>= index size)
           (error "index out of bounds" index)
           (+ addr index overhead))))
     (error "index must numerical" index)))
 
 (define (make addr)
   (cons vector-tag addr))

  (define (address vctr)
   (cdr vctr))
 
 (define (vector? any)
   (and (pair? any)
        (eq? (car any) vector-tag)))
 
 (define (make-vector size)
   (define index (locate-free (+ size overhead)))
   (if (eq? index null)
     null
     (let ((addr (pop-free index)))
       (if (eq? addr null)
         null
         (make addr)))))
 
 (define (vector-ref vctr index)
   (define addr (address vctr))
   (peek (address+index addr index)))
 
 (define (vector-set! vctr index any)
   (define addr (address vctr))
   (poke! (address+index addr index) any))
 
 (define (vector-length vctr)
   (define addr (address vctr))
   (define vsiz (size-free addr))
   (- vsiz overhead))
 
 (define (vector-free vctr)
   (define addr (address vctr))
   (push-free addr))
 
 (initialize))