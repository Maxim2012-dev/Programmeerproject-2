#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                    Generating Huffman Trees                     *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                  2009  Software Languages Lab                   *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (rnrs base) (rnrs io simple)
        (prefix (a-d priority-queue positional-list) pq:))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (huffman-tree frequencies)
  (define pq (pq:new <))
  (define (init-pq frequencies)
    (define pair (car frequencies))
    (pq:enqueue! pq pair (weight-leaf pair))
    (if (not (null? (cdr frequencies)))
      (init-pq (cdr frequencies))))
  (define (generate-trees)
    (let ((tree1 (pq:serve! pq)))
      (if (not (pq:empty? pq))
        (let ((tree2 (pq:serve! pq)))
          (let ((newt (make-code-tree tree1 tree2)))
            (pq:enqueue! pq newt (weight newt))
            (generate-trees)))
        tree1)))
  (init-pq frequencies)
  (generate-trees))

(define freqs (list (make-leaf 'c 10) (make-leaf 'g 4) (make-leaf 'd 8)
                    (make-leaf 'a 40) (make-leaf 'e 8) (make-leaf 'b 20)
                    (make-leaf 'f 6)  (make-leaf 'h 4)))
(define ht (huffman-tree freqs))