#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                 Scheme Vector Memory Manager                    *-*-
;-*-*        (with recursive mark&sweep garbage collection)           *-*-
;-*-*               Theo D'Hondt and Wolfgang De Meuter               *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (recursive)
 (export root! vector? vector-tag allocate overhead poke! peek make null address make)
 (import (except (rnrs base) vector?)
         (rnrs control))
 
 (define null '())
 
 (define overhead 1)
 (define vector-tag 'vector)
 (define memory-size 25)
 (define next-free 0)
 (define root null)
 
 (define (root! r)
   (set! root r))
 
 (define memory 
   (let
       ((vec (make-vector memory-size null)))
     (vector-set! vec 0 memory-size)
     vec))
 
 (define (poke! addr exp)
   (vector-set! memory addr exp))
 
 (define (peek addr)
   (vector-ref memory addr))
 
 (define (make addr)
   (cons vector-tag addr))
 
 (define (vector? any)
   (and (pair? any)
        (eq? (car any) vector-tag)))
 
 (define (address vector)
   (if (vector? vector)
     (cdr vector)
     (error "vector expected" vector)))
 
 (define (gc)
   (define (sweep item)
     (define addr (address item))
     (define size  (peek addr))
     (when (and (number? size) (positive? size))
       (poke! addr (- size))
       (do ((index 1 (+ index 1)))
         ((= index size))
         (let* 
             ((comp-addr (+ addr index))
              (comp (peek comp-addr)))
           (when (vector? comp)
             (sweep comp)
             (let* 
                 ((ref-addr (address comp))
                  (ref (peek ref-addr)))
               (poke! comp-addr ref)
               (poke! ref-addr (make comp-addr))))))))
   (define (collect)
     (let loop 
       ((addr 0)
        (new-addr 0))
       (if (< addr memory-size)
         (let*
             ((this-ref (peek addr))
              (size (do ()
                      ((number? this-ref) this-ref)
                      (let*
                          ((this-addr (address this-ref))
                           (next-ref (peek this-addr)))
                        (poke! this-addr (make new-addr))
                        (set! this-ref next-ref)))))
           (cond
             ((negative? size)
              (poke! addr size)
              (loop (- addr size) (- new-addr size)))
             (else
              (loop (+ addr size) new-addr)))))))
   (define (crunch)
     (let loop
       ((addr 0)
        (new-addr 0))
       (cond 
         ((< addr memory-size)
          (let ((size (- (peek addr))))
            (cond
              ((positive? size)
               (poke! new-addr size)
               (do ((index 1 (+ index 1)))
                 ((= index size))
                 (poke! (+ new-addr index)
                        (peek (+ addr index))))
               (loop (+ addr size) (+ new-addr size)))
              (else
               (loop (- addr size) new-addr)))))
         (else
          (if (< new-addr memory-size)
            (poke! new-addr (- memory-size new-addr)))
          (set! next-free new-addr)))))
   (if (vector? root)
     (sweep root))
   (collect)
   (crunch))
 
 (define (allocate size)
   (define boundary (- memory-size size))
   (if (> next-free boundary)
     (gc))
   (if (> next-free boundary)
     (error "storage overflow" size))
   (let ((addr next-free))
     (set! next-free (+ next-free size))
     (poke! addr size)
     (if (< next-free memory-size))
       (poke! next-free (- memory-size next-free)))
     addr)))