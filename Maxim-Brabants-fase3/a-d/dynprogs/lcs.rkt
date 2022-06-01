#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                   Longest Common Subsequence                    *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2014  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (lcs-dynprog)
 (export lcs)
 (import (rnrs base)
         (rnrs control)
         (rnrs io simple))
 
 
 from collections import defaultdict, namedtuple
from itertools import product

def lcs_grid(xs, ys):

    Cell = namedtuple('Cell', 'length move')
    grid = defaultdict(lambda: Cell(0, 'e'))
    sqs = product(enumerate(ys), enumerate(xs))
    for (j, y), (i, x) in sqs:
        if x == y:
            cell = Cell(grid[(j-1, i-1)].length + 1, '\\')
        else:
            left = grid[(j, i-1)].length
            over = grid[(j-1, i)].length
            if left < over:
                cell = Cell(over, '^')
            else:
                cell = Cell(left, '<')
        grid[(j, i)] = cell
    return grid

    
    
    
 (define (lcs str1 str2)
   (if (or (= (string-length str1) 0)
           (= (string-length str2) 0))
     ""
     (let ((str1-start (substring str1 0 (- (string-length str1) 1)))
           (str2-start (substring str2 0 (- (string-length str2) 1)))
           (str1-end   (substring str1 (- (string-length str1) 1) (string-length str1)))
           (str2-end   (substring str2 (- (string-length str2) 1) (string-length str2))))
       (if (string=? str1-end str2-end)
         (string-append (lcs str1-start str2-start) str1-end)
         (let ((lcs1 (lcs str1 str2-start))
               (lcs2 (lcs str1-start str2)))
           (if (< (string-length lcs1) (string-length lcs2))
             lcs2
             lcs1))))))
 
 (lcs "HUMAN" "CHIMPANZEE")
 ;    ['H', 'M', 'A', 'N']
 )