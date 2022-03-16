#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                    Comparative Sorting Test                     *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (rnrs base)
        (rnrs io simple)
        ;(a-d sorting internal comparative bubble-sort))
        ;(a-d sorting internal comparative insertion-sort))
        ;(a-d sorting internal comparative selection-sort))
        ;(a-d sorting internal comparative heapsort))
;(a-d sorting internal comparative quicksort))
;(a-d sorting internal comparative quicksort-m3))
(a-d sorting internal comparative quicksort-randomized))
;(a-d sorting internal comparative mergesort))

(define numbers (vector 5 20 21 22 10 23 29 2 28 24 11 27 7 26 25  19 1 14 0 16 9 13 8 17 6 18 3 12 4))
(sort numbers <)

(display numbers)(newline)

(define persons (vector (cons "Curtis" 41)
                        (cons "Anna-Mae" 44)
                        (cons "Selma" 23)
                        (cons "Kenneth" 68)
                        (cons "Roger" 41) 
                        (cons "Curtis" 22)
                        (cons "Curtis" 4)
                        (cons "Imogene" 22)
                        (cons "Imelda" 34)))

(display persons)(newline)

(sort persons (lambda (x y)
                (string<? (car x) (car y))))

(display persons)(newline)

(sort persons (lambda (x y)
                (< (cdr x) (cdr y))))

(display persons)(newline)