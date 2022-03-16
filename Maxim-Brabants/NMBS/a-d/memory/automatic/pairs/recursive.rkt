#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                   Scheme Pair Memory Manager                    *-*-
;-*-*         (with recursive mark&sweep garbage collection)          *-*-
;-*-*               Theo D'Hondt and Wolfgang De Meuter               *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (recursive)
 (export pair-tag car-poke! cdr-poke! car-peek cdr-peek allocate out-of-memory? gc tag untag null root!)
 (import (rename (rnrs base) (car scheme:car) (cdr scheme:cdr) 
                 (cons scheme:cons) (pair? scheme:pair?))
         (rnrs control))
 
 (define null ())
 
 (define memory-size 20)
 (define next-free 0)
 (define root null)
 
 (define (root! r)
   (set! root r))
 
 (define car-memory
   (do ((memory (make-vector memory-size null))
        (addr 1 (+ addr 1)))
     ((= addr memory-size ) memory)
     (vector-set! memory (- addr 1) addr)))
 
 (define cdr-memory
   (make-vector memory-size null))
 
 (define (car-poke! address val)
   (vector-set! car-memory address val))
 (define (cdr-poke! address val)
   (vector-set! cdr-memory address val))
 (define (car-peek address)
   (vector-ref car-memory address))
 (define (cdr-peek address)
   (vector-ref cdr-memory address))
 
 (define (out-of-memory?)
   (eq? next-free null))
 
 (define (allocate)
   (define addr next-free)
   (set! next-free (vector-ref car-memory addr))
   addr)
 
 (define pair-tag 'pair)
 
 (define (tag addr)
   (scheme:cons pair-tag addr))
 
 (define (untag pair)
   (if (scheme:pair? pair)
       (scheme:cdr pair)
       (error "pair expected" pair)))
 
 (define (gc)
   (define free-bits (make-vector memory-size #t))
   (define (mark pair)
     (if (scheme:pair? pair)
         (let ((addr (untag pair)))
           (when (vector-ref free-bits addr)
             (vector-set! free-bits addr #f)
             (mark (vector-ref car-memory addr))
             (mark (vector-ref cdr-memory addr))))))
   (define (sweep)
     (do ((addr 0 (+ addr 1)))
       ((= addr memory-size ))
       (when (vector-ref free-bits addr)
         (vector-set! car-memory addr next-free)
         (set! next-free addr))))
   (mark root)
   (sweep)))