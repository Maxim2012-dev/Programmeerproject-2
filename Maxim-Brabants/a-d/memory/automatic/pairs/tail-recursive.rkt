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
 (export pair-tag car-poke! car-peek cdr-poke! cdr-peek gc out-of-memory? tag untag allocate null root!)
 (import (rename (rnrs base) (car scheme:car) (cdr scheme:cdr) 
                 (cons scheme:cons) (pair? scheme:pair?))
         (rnrs control))
 
 (define null ())
 
 (define memory-size 10)
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
 
 (define (car-poke! addr val)
   (vector-set! car-memory addr val))
 (define (cdr-poke! addr val)
   (vector-set! cdr-memory addr val))
 (define (car-peek addr)
   (vector-ref car-memory addr))
 (define (cdr-peek addr)
   (vector-ref cdr-memory addr))
 
 (define count-mem (make-vector memory-size 0))
 (define (count! pair cnt)
   (vector-set! count-mem (untag pair) cnt))
 (define (count pair)
   (vector-ref count-mem (untag pair)))
 (define (descend? item)
   (and (scheme:pair? item)
        (zero? (count item))))

 (define (out-of-memory?)
   (null? next-free))
 
 (define (allocate)
   (define addr next-free)
   (set! next-free (car-peek addr))
   addr)
 
 (define pair-tag 'pair)
 
 (define (tag addr)
   (scheme:cons pair-tag addr))
 
 (define (untag pair)
   (if (scheme:pair? pair)
     (scheme:cdr pair)
     (error "pair expected" pair)))
 
 (define (gc)
   (define (mark prev curr)
     (case (count curr)
       ((0)
        (count! curr 1)
        (let ((hold (car-peek (untag curr))))
          (cond 
            ((descend? hold)
             (car-poke! (untag curr) prev)
             (mark curr hold))
            (else
             (mark prev curr)))))
       ((1)
        (count! curr 2)
        (let ((hold (cdr-peek (untag curr))))
          (cond 
            ((descend? hold)
             (cdr-poke! (untag curr) prev)
             (mark curr hold))
            (else
             (mark prev curr)))))
       ((2)
        (if (not (null? prev))
          (case (count prev)
            ((1)
             (let ((hold (car-peek (untag prev))))
               (car-poke! (untag prev) curr)
               (mark hold prev)))
            ((2)
             (let ((hold (cdr-peek (untag prev))))
               (cdr-poke! (untag prev) curr)
               (mark hold prev))))))))
   (define (sweep)
     (do ((addr 0 (+ addr 1)))
       ((= addr memory-size))
       (when (zero? (vector-ref count-mem addr))
         (vector-set! car-memory addr next-free)
         (set! next-free addr))
       (vector-set! count-mem addr 0)))
   (unless (null? root)
     (mark () root))
   (sweep)))