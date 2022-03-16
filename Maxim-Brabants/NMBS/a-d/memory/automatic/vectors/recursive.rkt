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
 (export root! vector? vector-tag allocate overhead poke! peek tag null untag)
 (import (rename (rnrs base) (pair? scheme:pair?) (make-vector scheme:make-vector)
                 (vector? scheme:vector?) (vector-set! scheme:vector-set!)
                 (vector-ref scheme:vector-ref))
         (rnrs control))
 
 (define null ())
 
 (define overhead 1)
 (define memory-size 25)
 
 (define memory 
   (let
       ((m (scheme:make-vector memory-size null)))
     (scheme:vector-set! m 0 memory-size)
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
 
 (define (untag vector)
   (if (vector? vector)
     (cdr vector)
     (error "vector expected" vector)))
 
 (define (vector? any)
   (and (scheme:pair? any)
        (eq? (car any) vector-tag)))
 
 (define (gc)
   (define (mark curr)
     (define addr (untag curr))
     (define size (peek addr))
     (when (and (number? size) (> size 0))
       (poke! addr (- size))
       (do ((indx 1 (+ indx 1)))
         ((= indx size))
         (let* 
             ((comp-addr (+ addr indx))
              (comp (peek comp-addr)))
           (when (vector? comp)
             (mark comp)
             (let* 
                 ((head-addr (untag comp))
                  (head (peek head-addr)))
               (poke! comp-addr head)
               (poke! head-addr (tag comp-addr))))))))
   
   (define (sweep-prepare)
     (let loop 
       ((addr 0)
        (new-addr 0))
       (if (< addr memory-size)
         (let*
             ((size (let traverse
                      ((curr (peek addr)))
                      (if (number? curr)
                        curr
                        (let*
                            ((curr-addr (untag curr))
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
       (cond 
         ((< addr memory-size)
          (let ((size (- (peek addr))))
            (cond
              ((> size 0)
               (poke! new-addr size)
               (do ((index 1 (+ index 1)))
                 ((= index size))
                 (poke! (+ new-addr index)
                        (peek (+ addr index))))
               (if (= addr (untag root))
                 (set! root (tag new-addr)))
               (loop (+ addr size) (+ new-addr size)))
              (else
               (loop (- addr size) new-addr)))))
         (else
          (if (< new-addr memory-size)
            (poke! new-addr (- memory-size new-addr)))
          (set! next-free new-addr)))))
   (if (vector? root)
     (mark root))
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
     (set! next-free (+ next-free size))
     (poke! addr size)
     (if (< next-free memory-size)
       (poke! next-free (- memory-size next-free)))
     addr)))