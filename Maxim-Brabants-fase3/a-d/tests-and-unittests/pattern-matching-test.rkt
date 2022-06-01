#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                    Pattern Matching Tests                       *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (rnrs base)
        (rnrs io simple)
        ;(a-d pattern-matching brute-force))
        ;(a-d pattern-matching boyer-moore))
        ;(a-d pattern-matching horspool))
        ;(a-d pattern-matching kmp))
        (a-d pattern-matching quicksearch))
        
(display (match "abacaabaccabacabaabbc" "abacaa"))(newline)
(display (match "abacaabaccabacabaabbc" "aabbc"))(newline)
(display (match "abacaabaccabacabaabbc" "caa"))(newline)
(display (match "abacaabaccabacabaabbc" "c"))(newline)
(display (match "abacaabaccabacabaabbc" "v"))(newline)
(display (match "abacaabaccabacabaabbc" "baabbe"))(newline)
(display (match "abacaabaccabacabaabbc" "baabbce"))(newline)
(display (match "abacaabaccabacabaabbc" "efgh"))(newline)
(display (match "abracadabra" "abracadabra"))(newline)
(display (match "lalaland" "lalaland"))