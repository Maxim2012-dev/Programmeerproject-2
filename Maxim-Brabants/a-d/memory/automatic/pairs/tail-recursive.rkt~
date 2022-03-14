#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                   Scheme Pair Memory Manager                    *-*-
;-*-*         (with iterative mark&sweep garbage collection)          *-*-
;-*-*               Theo D'Hondt and Wolfgang De Meuter               *-*-
;-*-*                 2007 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (tail-recursive)
 (export pair-tag car-poke! car-peek cdr-poke! cdr-peek gc out-of-memory? make address allocate null root!)
 (import (rnrs base)
         (rnrs control)
         (rnrs mutable-pairs))
 
 (define null '())
 
 (define memory-size 5)
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
     (error "pair expected" address)))
 
 (define (gc)
   (define tags (make-vector memory-size 0))
   (define (tag! item value)
     (vector-set! tags (address item) value))
   (define (tag item)
     (vector-ref tags (address item)))
   
   (define (free? item)
     (if (pair? item)
       (zero? (tag item))
       #f))
   
   (define (sweep previous current)
     (case (tag current)
       ((0)
        (tag! current 1)
        (let ((hold (car current)))
          (cond 
            ((free? hold)
             (set-car! current previous)
             (sweep current hold))
            (else
             (sweep previous current)))))
       ((1)
        (tag! current 2)
        (let ((hold (cdr current)))
          (cond 
            ((free? hold)
             (set-cdr! current previous)
             (sweep current hold))
            (else
             (sweep previous current)))))
       ((2)
        (if (pair? previous)
          (case (tag previous)
            ((1)
             (let ((hold (car previous)))
               (set-car! previous current)
               (sweep hold previous)))
            ((2)
             (let ((hold (cdr previous)))
               (set-cdr! previous current)
               (sweep hold previous))))))))
   
   (define (collect)
     (do ((addr 0 (+ addr 1)))
       ((= addr memory-size ))
       (when (zero? (vector-ref tags addr))
         (vector-set! car-memory addr next-free)
         (set! next-free addr))))
   
   (if (pair? root)
     (sweep '() root))
   (collect)))