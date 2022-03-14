#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                  Non-comparative Sorting Test                   *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (rnrs base)
        (rnrs mutable-pairs)
        (rnrs io simple)
        (a-d sorting internal non-comparative counting-sort)
        (a-d sorting internal non-comparative radix-sort))

(define (person n nr) (cons n nr))
 (define key cdr)
 
 (define in (vector (person "wolf" 3)
                    (person "Anni" 6) 
                    (person "Thelma" 4)
                    (person "Yann" 1)
                    (person "Steven" 3)
                    (person "Peter" 4) 
                    (person "Linda" 1) 
                    (person "Theo" 4)))
 (display in)(newline)
 (define out (make-vector 8 #f))
 (counting-sort in out 7 key)
 (display out)(newline)
 
 (define age cdr)
 (define l (list (person "wolf" 34)
                 (person "jon" 13)
                 (person "tom" 22)
                 (person "marjet" 66)
                 (person "kevin" 23)
                 (person "Janneke" 23)
                 (person "mieke" 20)
                 (person "joseph" 76)
                 (person "sven" 28)
                 (person "rudy" 34)))
 (display (radix-sort l age 2))(newline)
 