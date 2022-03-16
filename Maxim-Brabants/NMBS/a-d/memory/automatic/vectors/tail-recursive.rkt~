#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                 Scheme Vector Memory Manager                    *-*-
;-*-*        (with iterative mark&sweep garbage collection)           *-*-
;-*-*               Theo D'Hondt and Wolfgang De Meuter               *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (tail-recursive)
 (export vector? root! overhead allocate poke! peek make null address)
 (import (except (rnrs base) vector?)
         (rnrs control))
 
 (define null '())
 
 (define overhead 2)
 (define vector-tag 'vector)
 (define memory-size 25)
 (define next-free 0)
 (define root null)
 
 (define memory 
   (let ((vector (make-vector memory-size null)))
     (vector-set! vector 0 memory-size)
     (vector-set! vector 1 memory-size)
     vector))
 
 (define (poke! addr exp)
   (vector-set! memory addr exp))
 
 (define (peek addr)
   (vector-ref memory addr))
 
 (define (make addr)
   (cons vector-tag addr))

  (define (root! r)
   (set! root r))
 
 (define (vector? any)
   (and (pair? any)
        (eq? (car any) vector-tag)))
 
 (define (address vector)
   (if (vector? vector)
     (cdr vector)
     (error "vector expected" vector)))
 
 (define (gc)
   (define (sweep previous current)
     (define addr (address current))
     (define index  (peek addr))    
     (define size   (peek (+ addr 1)))
     (define (test-and-set! item)
       (if (vector? item)
         (let*
             ((addr (address item))
              (size (peek (+ addr 1))))
           (if (number? size)
             (cond
               ((positive? size)
                (poke! (+ addr 1) (- size))
                #t)
               (else #f))
             #f))
         #f))
     (cond
       ((> index 2)
        (let* 
            ((comp-addr (+ addr index -1))
             (comp (peek comp-addr)))
          (poke! addr (- index 1))
          (cond 
            ((test-and-set! comp)
             (poke! comp-addr previous)
             (sweep current comp))
            (else
             (let* 
                 ((ref-addr (+ (address comp) 1))
                  (ref (peek ref-addr))) 
               (poke! comp-addr ref)
               (poke! ref-addr (make comp-addr))
               (sweep previous current))))))
       ((vector? previous)
        (let*
            ((p-addr (address previous))
             (p-index (peek p-addr))
             (p-size  (peek (+ p-addr 1)))
             (comp-addr (+ p-addr p-index 0))
             (comp (peek comp-addr)))
          (poke! comp-addr size)
          (poke! (+ addr 1) (make comp-addr))
          (sweep comp previous)))))
   (define (collect)
     (let loop 
       ((addr 0)
        (new-addr 0))
       (if (< (+ addr 1) memory-size)
         (let*
             ((this-ref (peek (+ addr 1)))
              (size (do ()
                      ((number? this-ref) this-ref)
                      (let*
                          ((this-addr (address this-ref))
                           (next-ref (peek this-addr)))
                        (poke! this-addr (make new-addr))
                        (set! this-ref next-ref)))))
           (cond
             ((negative? size)
              (poke! (+ addr 1) size)
              (loop (- addr size) (- new-addr size)))
             (else
              (loop (+ addr size) new-addr)))))))
   (define (crunch)
     (let loop
       ((addr 0)
        (new-addr 0))
       (if (< (+ addr 1) memory-size)
         (let 
             ((size (- (peek (+ addr 1)))))
           (cond
             ((positive? size)
              (poke! new-addr size)
              (poke! (+ new-addr 1) size)
              (do ((index 2 (+ index 1)))
                ((> index size))
                (poke! (+ new-addr index)
                               (peek (+ addr index))))
              (loop (+ addr size) (+ new-addr size)))
             (else
              (loop (- addr size) new-addr))))
         (set! next-free new-addr))))
   (if (vector? root)
     (sweep '() root))
   (collect)
   (crunch))
 
 (define (allocate size)
   (define boundary (- memory-size size))
   (if (> next-free boundary)
     (gc))
   (if (> next-free boundary)
     (error "storage overflow" size))
   (let ((addr next-free))
     (set! next-free (+ next-free size 0))
     (poke! addr size)
     (poke! (+ addr 1) size)
     (when (< next-free (- memory-size 1))
       (poke! next-free (- memory-size next-free))
       (poke! (+ next-free 1) (- memory-size next-free)))
     addr)))