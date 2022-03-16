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
 (export pair-tag car-poke! cdr-poke! car-peek cdr-peek address allocate out-of-memory? gc make null root!)
 (import (rnrs base)
         (rnrs control)
         (rnrs mutable-pairs))
 
 (define null '())
 
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
 
 (define (make addr)
   (cons pair-tag addr))
 
 (define (address pair)
   (if (pair? pair)
       (cdr pair)
       (error "pair expected" pair)))
 
 (define (gc)
   (define free-bits (make-vector memory-size #t))
   (define (sweep pair)
     (if (pair? pair)
         (let ((addr (address pair)))
           (when (vector-ref free-bits addr)
             (vector-set! free-bits addr #f)
             (sweep (vector-ref car-memory addr))
             (sweep (vector-ref cdr-memory addr))))))
   (define (collect)
     (do ((addr 0 (+ addr 1)))
       ((= addr memory-size ))
       (when (vector-ref free-bits addr)
         (vector-set! car-memory addr next-free)
         (set! next-free addr))))
   (sweep root)
   (collect)))