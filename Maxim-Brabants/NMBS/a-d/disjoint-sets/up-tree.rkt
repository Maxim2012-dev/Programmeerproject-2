#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*              Disjoint Sets (Up-Tree Implementation)             *-*-
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
 (import (srfi :9)
         (rnrs base))
 
 (define-record-type disjoint-sets
   (make t)
   disjoint-sets?
   (t up-trees))
 
 (define (new size)
   (define trees (make-vector size 0))
   (define sets (make trees))
   (let fill-singletons 
     ((i 0))
     (vector-set! trees i i)
     (if (< (+ 1 i) size)
       (fill-singletons (+ i 1))))
   sets)
 
 (define same-set? =)
 
 (define (find sets nmbr)
   (define trees (up-trees sets))
   (let up-tree-loop
     ((elmt nmbr))
     (if (= elmt (vector-ref trees elmt))
       elmt
       (up-tree-loop (vector-ref trees elmt)))))
 
 (define (union! sets set1 set2)
   (define trees (up-trees sets))
   (vector-set! trees set1 set2)
   sets))