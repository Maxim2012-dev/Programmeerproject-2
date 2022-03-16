#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                    Binary Trees (Examples)                      *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (rnrs base)
        (rnrs io simple)
        (a-d tree binary-tree)
        (a-d tree binary-tree-algorithms))

(define t4 (new 4 null-tree null-tree))
(define t9 (new 9 null-tree null-tree))
(define t5 (new 5 null-tree null-tree))
(define t6 (new 6 null-tree null-tree))
(define minus (new '- t4 t9))
(define plus (new '+ t5 t6))
(define times (new '* minus plus))


(define (eval tree)
  (cond ((eq? (value tree) '+) 
         (+ (eval (left tree))
            (eval (right tree))))
        ((eq? (value tree) '*) 
         (* (eval (left tree))
            (eval (right tree))))
        ((eq? (value tree) '-) 
         (- (eval (left tree))
            (eval (right tree))))
        (else (value tree))))

(in-order times display)(newline)
(iterative-in-order times  display)(newline)
(pre-order times display)(newline)
(iterative-pre-order times display)(newline)
(post-order times display)(newline)
(iterative-post-order times  display)(newline)
(iterative-post-order2 times  display)(newline)
(breadth-first times  display)(newline)

(define a-tree
  (new 3
       (new 40 
            (new 34
                 null-tree
                 null-tree)
            null-tree)
       (new 77
            null-tree 
            null-tree)))

(define boom
  (new 3
       null-tree
       (new 40
            null-tree
            (new 22
                 null-tree
                 (new 77 
                      null-tree
                      (new 34 
                           null-tree 
                           null-tree))))))

(define arbre
  (new 1
       null-tree
       (new 2
            (new 3
                 null-tree
                 (new 4
                      null-tree
                      null-tree))
            null-tree)))

(define arbol
  (new 3
       (new 40 null-tree (new 22 null-tree null-tree) )
       (new 77 (new 34 null-tree null-tree) null-tree)))

(define dyerevo
  (new 3
       (new 40 
            (new 22
                 (new 77 
                      (new 34 
                           null-tree 
                           null-tree)
                      null-tree)
                 null-tree)
            null-tree)
       null-tree))

(iterative-post-order a-tree (lambda (x) (display x) (display "-")))(newline)
(iterative-post-order arbol (lambda (x) (display x) (display "-")))(newline)
(iterative-post-order times  display)(newline)
(iterative-post-order dyerevo (lambda (x) (display x) (display "-")))(newline)
(iterative-post-order arbre (lambda (x) (display x) (display "-")))(newline)
(iterative-post-order boom (lambda (x) (display x) (display "-")))(newline)
