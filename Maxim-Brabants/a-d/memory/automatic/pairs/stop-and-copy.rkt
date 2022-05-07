#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                   Scheme Pair Memory Manager                    *-*-
;-*-*               (with stop&copy garbage collection)               *-*-
;-*-*               Theo D'Hondt and Wolfgang De Meuter               *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (stop-and-copy)
 (export car-poke! car-peek cdr-poke! cdr-peek out-of-memory? tag gc untag allocate pair-tag null root!)
 (import (rename (rnrs base) (car scheme:car) (cdr scheme:cdr) 
                 (cons scheme:cons) (pair? scheme:pair?)))
 
 (define null ())
 
 (define memory-size 10)
 (define next-free 0)
 (define root null)
 
 (define car-memory (make-vector memory-size null))
 (define cdr-memory (make-vector memory-size null))
 
 (define car-memory-2 (make-vector memory-size null))
 (define cdr-memory-2 (make-vector memory-size null))

 (define (car-poke! address val)
   (vector-set! car-memory address val))
 (define (cdr-poke! address val)
   (vector-set! cdr-memory address val))
 (define (car-peek address)
   (vector-ref car-memory address))
 (define (cdr-peek address)
   (vector-ref cdr-memory address))

 (define (root! r)
   (set! root r))

 (define (out-of-memory?)
   (eq? next-free memory-size))
 
 (define (allocate)
   (define addr next-free)
   (set! next-free (+ 1 next-free))
   addr)
 
 (define pair-tag 'pair)

 (define (tag addr)
   (scheme:cons pair-tag addr))
 
 (define (untag pair)
   (scheme:cdr pair))
 
 (define forward 'forward)

 (define (gc)
   (define old-car-memory car-memory)
   (define old-cdr-memory cdr-memory)
   
   (define (move old-pair)
     (if (scheme:pair? old-pair)
       (let* 
           ((old-addr (untag old-pair))
            (old-car (vector-ref old-car-memory old-addr))
            (old-cdr (vector-ref old-cdr-memory old-addr)))
         (if (eq? old-car forward)
           old-cdr
           (let*
               ((new-addr next-free)
                (new-pair (tag new-addr)))
             (set! next-free (+ next-free 1))
             (vector-set! old-car-memory old-addr forward)
             (vector-set! old-cdr-memory old-addr new-pair)
             (vector-set! car-memory new-addr old-car)
             (vector-set! cdr-memory new-addr old-cdr)
             new-pair)))
       old-pair))
   (define (scan addr)
     (if (< addr next-free)
       (let 
           ((old-car (vector-ref car-memory addr))
            (old-cdr (vector-ref cdr-memory addr)))
         (vector-set! car-memory addr (move old-car))
         (vector-set! cdr-memory addr (move old-cdr))
         (scan (+ addr 1)))))
   (set! car-memory car-memory-2)
   (set! cdr-memory cdr-memory-2)  
   (set! next-free 0)
   (set! root (move root))
   (scan 0)
   (set! car-memory-2 old-car-memory)
   (set! cdr-memory-2 old-cdr-memory)))