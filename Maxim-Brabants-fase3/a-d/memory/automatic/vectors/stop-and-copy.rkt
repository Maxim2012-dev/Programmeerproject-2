#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                 Scheme Vector Memory Manager                    *-*-
;-*-*              (with stop&copy garbage collection)                *-*-
;-*-*               Theo D'Hondt and Wolfgang De Meuter               *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (stop-and-copy)
 (export root! overhead vector? allocate poke! peek tag null untag)
 (import (rename (rnrs base) (pair? scheme:pair?)
                 (make-vector scheme:make-vector) (cdr scheme:cdr)
                 (vector? scheme:vector?) (vector-set! scheme:vector-set!)
                 (vector-ref scheme:vector-ref))
         (rnrs control))
 
 (define null ())
 
 (define overhead 1)
 (define memory-size 25)

 (define root null)
 (define new-root! ())
 (define (root! r notify)
   (set! root r)
   (set! new-root! notify))
 
 (define memory (scheme:make-vector memory-size null))
 
 (define (poke! addr value)
   (scheme:vector-set! memory addr value))
 
 (define (peek addr)
   (scheme:vector-ref memory addr))
 
 (define old-memory (scheme:make-vector memory-size null))
 
 (define (poke-old! addr exp)
   (scheme:vector-set! old-memory addr exp))
 
 (define (peek-old addr)
   (scheme:vector-ref old-memory addr))
 
 (define forward-tag 'forward)

 (define (forward? exp)
   (and (scheme:pair? exp)
        (eq? (car exp) forward-tag)))
 (define (make-forward addr)
   (cons forward-tag addr))
 (define (forward-address fwd)
   (scheme:cdr fwd))
 
 (define vector-tag 'vector)

 (define (tag addr)
   (cons vector-tag addr))
 
 (define (untag vector)
   (if (vector? vector)
     (scheme:cdr vector)
     (error "vector expected" vector)))
  
 (define (vector? any)
   (and (scheme:pair? any)
        (eq? (car any) vector-tag)))
 
 (define (gc)
   (define hold-memory memory)
   
   (define (move old-vector)
     (if (vector? old-vector)
       (let* 
           ((old-addr (untag old-vector))
            (old-size (peek-old old-addr)))
         (if (forward? old-size)
           (tag (forward-address old-size))
           (let*
               ((addr next-free)
                (newv (tag addr)))
             (set! next-free (+ next-free old-size))
             (poke-old! old-addr (make-forward addr))
             (poke! addr old-size)
             (do ((index 1 (+ index 1)))
               ((>= index old-size) newv)
               (poke! (+ addr index) (peek-old (+ old-addr index)))))))
       old-vector))
   
   (define (scan addr)
     (if (< addr next-free)
       (let
           ((size (peek addr)))
         (do ((index 1 (+ index 1)))
           ((>= index size))
           (poke! (+ addr index) (move (peek (+ addr index)))))
         (scan (+ addr size)))))
   (set! memory old-memory)
   (set! old-memory hold-memory)
   (set! next-free 0)
   (set! root (move root))
   (new-root! root)
   (scan 0))
 
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
     addr)))