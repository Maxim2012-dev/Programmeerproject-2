#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                 Disjoint Sets (Path Compression)                *-*-
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
   (make t r)
   disjoint-sets?
   (t up-trees)
   (r tree-ranks))
 
 (define (new size)
   (define trees (make-vector size 0))
   (define ranks (make-vector size 0))
   (define sets (make trees ranks))
   (let fill-singletons 
     ((i 0))
     (vector-set! trees i i)
     (if (< (+ 1 i) size)
       (fill-singletons (+ i 1))))
   sets)
 
 (define same-set? =)
 
 (define (find sets nmbr)
   (define trees (up-trees sets))
   (define (up-tree-rec elmt)
     (if (not (eq? elmt (vector-ref trees elmt)))
       (vector-set! trees elmt (up-tree-rec (vector-ref trees elmt))))
     (vector-ref trees elmt))
   (up-tree-rec nmbr))
 
 (define (union! sets set1 set2) ; set1 en set2 zijn leiders!
   (define ranks (tree-ranks sets))
   (define trees (up-trees sets))
   (cond ((> (vector-ref ranks set1) 
             (vector-ref ranks set2))
          (vector-set! trees set2 set1))
         ((= (vector-ref ranks set1) 
             (vector-ref ranks set2))
          (vector-set! trees set1 set2)
          (vector-set! ranks set2 (+ 1 (vector-ref ranks set2))))
         (else
          (vector-set! trees set1 set2)))
   sets))

; Voorbeeld: unie van x en y
;; (union! s (find s x) (find s y))