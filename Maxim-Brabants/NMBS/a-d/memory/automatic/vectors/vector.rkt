#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                   Scheme Vector Memory Manager                  *-*-
;-*-*                                                                 *-*-
;-*-*               Theo D'Hondt and Wolfgang De Meuter               *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (vector)
 (export root! vector? make-vector vector-ref vector-set! vector-length)
 (import (except (rnrs base) vector? vector-ref vector-set! make-vector vector-length)
         (rnrs control)
         ;(a-d memory automatic vectors recursive))
         (a-d memory automatic vectors tail-recursive))
        ; (a-d memory automatic vectors stop-and-copy))
  
 (define (make-vector leng)
   (define addr (allocate (+ leng overhead)))
   (do ((index overhead (+ index 1)))
     ((>= index (+ leng overhead)))
     (poke! (+ addr index) null))
   (tag addr))
 
 (define (vector-set! vctr indx any)
   (define addr (untag vctr))
   (define leng (- (peek addr) overhead))
   (if (or (< indx 0) (>= indx leng))
     (error "illegal index" vector-set!))
   (poke! (+ addr indx overhead) any))
 
 (define (vector-ref vctr indx)
   (define addr (untag vctr))
   (define leng (- (peek addr) overhead))
   (if (or (< indx 0) (>= indx leng))
     (error "illegal index" vector-ref))
   (peek (+ addr indx overhead)))
 
 (define (vector-length vector)
   (define addr (untag vector))
   (- (peek addr) overhead)))