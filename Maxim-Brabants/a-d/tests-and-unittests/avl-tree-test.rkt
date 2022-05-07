#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                         AVL Trees Test                          *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import
 (rnrs base)
 (srfi \x32;7) ;import integer-random
 (rnrs io simple)
 (a-d tree avl-tree)
 (a-d tree binary-tree-algorithms))

(define (check-avl avl)
  (define (inner node)
    (if (null-tree? node)
      0
      (let ((AVL-node-left (inner (left node)))
            (AVL-node-right (inner (right node))))
        (if (and (eq? (balance node) 'AVL-node-balanced)
                 (not (=  AVL-node-left AVL-node-right)))
          (display "OH NO"))
        (if (and (eq? (balance node) 'Lhigh)
                 (not (= AVL-node-left (+ AVL-node-right 1))))
            (display "Oh NO (L)"))
        (if (and (eq? (balance node) 'Rhigh)
                 (not (= (+ AVL-node-left 1)  AVL-node-right)))
          (display "Oh NO (R)"))
        (+ 1 (max AVL-node-left  AVL-node-right)))))
  (inner (root avl)))

(define (check-bst avl)
  (define (inner node)
    (if (null-tree? node)
      #t
      (let* ((lft (inner (left node)))
             (rgh (inner (right node)))
             (Lv (if (not (null-tree? (left node)))
                   (value (left node))
                   -inf.0))
             (Rv (if (not (null-tree? (right node)))
                   (value (right node))
                   +inf.0)))
        (and (< Lv (value node))
             (< (value node) Rv)
             lft
             rgh))))
  (inner (root avl)))

(define (count avl)
  (define (inner node)
    (if (null-tree? node)
      0
      (let* ((AVL-node-left (inner (left node)))
             (AVL-node-right (inner (right node))))
        (+ 1 AVL-node-left AVL-node-right))))
  (inner (root avl)))



(define (random-inbetween l r)
  (+ l (random-integer (+ (- r l) 1))))

(define a (new = <))
(insert! a 10)
(insert! a 20)
(insert! a 30)
(insert! a 40)
(define (iter b n)
  (if (= n 0)
    (display "done")
    (begin (insert! b (random-inbetween 10 10000))
           (iter b (- n 1)))))

(define (reti b n)
  (define count 0)
  (define (rreti n)
    (if (= n 0)
      (display "done")
      (let ((val (random-inbetween 10 10000)))
        (if (find b val)
          (begin (set! count (+ 1 count))
                 (delete! b val)))
        (rreti (- n 1)))))
  (rreti n)
  count)
(iter a 10000)
(check-avl a)
(check-bst a)
(reti a 5000)
(check-avl a)
(check-bst a)