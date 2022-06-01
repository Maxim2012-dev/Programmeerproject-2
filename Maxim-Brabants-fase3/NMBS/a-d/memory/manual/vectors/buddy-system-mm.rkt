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
        ; (a-d memory manual vectors exponential (2)))
         (a-d memory manual vectors fibonacci))
 
 (define vector-tag 'vector)
 
 (define (address+index addr index)
   (let
       ((size (- (size-free addr) overhead)))
     (if (or (< size 0)
             (>= index size))
       (error "index out of bounds" index)
       (+ addr index overhead))))
 
 (define (tag addr)
   (cons vector-tag addr))

  (define (untag vctr)
   (cdr vctr))
 
 (define (vector? any)
   (and (pair? any)
        (eq? (car any) vector-tag)))
 
 (define (make-vector size)
   (define index (locate-free (+ size overhead)))
   (if (null? index)
     null
     (let ((addr (pop-free index)))
       (if (null? addr)
         null
         (tag addr)))))
 
 (define (vector-ref vctr index)
   (define addr (untag vctr))
   (peek (address+index addr index)))
 
 (define (vector-set! vctr index any)
   (define addr (untag vctr))
   (poke! (address+index addr index) any))
 
 (define (vector-length vctr)
   (define addr (untag vctr))
   (define vsiz (size-free addr))
   (- vsiz overhead))
 
 (define (vector-free vctr)
   (define addr (untag vctr))
   (push-free addr))
 
 (initialize))