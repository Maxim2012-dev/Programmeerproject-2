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
 (export vector? root! overhead allocate poke! peek untag null tag)
 (import (rename (rnrs base) (pair? scheme:pair?) (make-vector scheme:make-vector) (cdr scheme:cdr)
                 (vector? scheme:make-vector?) (vector-ref scheme:vector-ref)
                 (vector-set! scheme:vector-set!))
         (rnrs control))
 
 (define null ())
 
 (define overhead 2)
 (define memory-size 31)
 
 (define memory 
   (let ((m (scheme:make-vector memory-size null)))
     (scheme:vector-set! m 0 memory-size)
     (scheme:vector-set! m 1 memory-size)
     m))
 
 (define (poke! addr value)
   (scheme:vector-set! memory addr value))
 
 (define (peek addr)
   (scheme:vector-ref memory addr))
 
 (define root null)
 
 (define new-root! ())
 (define (root! r notify)
   (set! root r)
   (set! new-root! notify))
 
 (define vector-tag 'vector)
 
 (define (tag addr)
   (cons vector-tag addr))
 
 (define (untag vctr)
   (if (vector? vctr)
     (scheme:cdr vctr)
     (error "vector expected" vctr)))
 
 (define (vector? any)
   (and (scheme:pair? any)
        (eq? (car any) vector-tag)))
 
 (define (descend? item)
   (if (vector? item)
     (let*
         ((addr (untag item))
          (size (peek addr)))
       (if (number? size)
         (cond
           ((> size 0)
            (poke! addr (- size))
            #t)
           (else #f))
         #f))
     #f))
      
 (define (gc)
   (define (mark prev curr)
     (define addr (untag curr))
     (define size (peek addr))    
     (define indx (peek (+ addr 1)))
     (cond
       ((> indx 2)
        (let* 
            ((comp-addr (+ addr indx -1))
             (comp (peek comp-addr)))
          (poke! (+ addr 1) (- indx 1))
          (cond 
            ((descend? comp)
             (poke! comp-addr prev)
             (mark curr comp))
            ((vector? comp)
             (let* 
                 ((head-addr (untag comp))
                  (head (peek head-addr))) 
               (poke! comp-addr head)
               (poke! head-addr (tag comp-addr))
               (mark prev curr)))
            (else
             (mark prev curr)))))
       ((vector? prev)
        (let*
            ((prev-addr (untag prev))
             (prev-size (peek prev-addr))
             (prev-indx (peek (+ prev-addr 1)))
             (comp-addr (+ prev-addr prev-indx)) ; prev-indx already -1
             (prev-prev (peek comp-addr)))
          (poke! comp-addr size)
          (poke! addr (tag comp-addr))
          (mark prev-prev prev)))))
   
   (define (sweep-prepare)
     (let loop 
       ((addr 0)
        (new-addr 0))
       (if (< (+ addr 1) memory-size)
         (let ((size (let traverse
                       ((curr (peek addr)))
                       (if (number? curr)
                         curr
                         (let* ((curr-addr (untag curr))
                                (next (peek curr-addr)))
                           (poke! curr-addr (tag new-addr))
                           (traverse next))))))
           (cond
             ((negative? size)
              (poke! addr size)
              (loop (- addr size) (- new-addr size)))
             (else
              (loop (+ addr size) new-addr)))))))
   
   (define (sweep-crunch)
     (let loop
       ((addr 0)
        (new-addr 0))
       (if (< (+ addr 1) memory-size)
         (let 
             ((size (- (peek addr))))
           (cond
             ((> size 0)
              (poke! new-addr size)
              (poke! (+ new-addr 1) size)
              (do ((index 2 (+ index 1)))
                ((= index size))
                (poke! (+ new-addr index)
                       (peek (+ addr index))))
              (if (= addr (untag root))
                (set! root (tag new-addr)))
              (loop (+ addr size) (+ new-addr size)))
             (else
              (loop (- addr size) new-addr))))
         (set! next-free new-addr))))
   (if (vector? root)
     (mark () root))
   (sweep-prepare)
   (sweep-crunch)
   (new-root! root))
 
 (define next-free 0)
 
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
     (when (< (+ next-free 1) memory-size)
       (poke! next-free (- memory-size next-free))
       (poke! (+ next-free 1) (- memory-size next-free)))
     addr)))