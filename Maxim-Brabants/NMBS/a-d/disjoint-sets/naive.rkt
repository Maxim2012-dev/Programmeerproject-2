#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                  Disjoint Sets (Naive Version)                  *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2012 Software Languages Lab                   *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (disjoint-sets)
 (export new disjoint-sets? find union! same-set?)
 (import (rnrs base)
         (srfi :9)
         (rnrs control))
 
 (define-record-type disjoint-sets
   (make i)
   disjoint-sets?
   (i identities))
 
 (define (new size)
   (define idts (make-vector size 0))
   (define sets (make idts))
   (let fill-singletons 
     ((i 0))
     (vector-set! idts i i)
     (if (< (+ 1 i) size)
       (fill-singletons (+ i 1))))
   sets)
 
 (define same-set? =) 

 (define (find sets nmbr)
   (define idts (identities sets))
   (vector-ref idts nmbr))
 
 (define (union! sets set1 set2)
   (define idts (identities sets))
   (define size (vector-length idts))
   (do ((i 0 (+ i 1)))
     ((= i size) sets)
     (if (= (vector-ref idts i) set1)
       (vector-set! idts i set2)))))