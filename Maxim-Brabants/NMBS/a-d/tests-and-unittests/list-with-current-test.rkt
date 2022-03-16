#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                    List with Current Tests                      *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import
 (except (rnrs base) length map)
 (a-d list-with-current linked)
 (only (scheme base) require planet)
 (rnrs io simple))

(require (planet schematics/schemeunit:3/test))

(define snumbers '(1 2 3 4 5 6 7))
(define numbers (from-scheme-list snumbers =))
(define inverses (map numbers (lambda (x) (- x)) =))

;(check-equal? (foldl numbers (lambda (x y) (+ x y)) 0) (apply + snumbers))
;(check-equal? (foldr numbers (lambda (x y) (- x y)) 0) 4)

(find! numbers 11)

(check-equal? (has-current? numbers) #f)

(set-current-to-first! numbers)

(check-equal? (has-current? numbers) #t)
(check-equal? (peek numbers) 1)
(check-equal? (length numbers) 7)

(check-equal? (current-has-next? numbers) #t)
(check-equal? (current-has-previous? numbers) #f)

(set-current-to-last! numbers)

(check-equal? (has-current? numbers) #t)
(check-equal? (peek numbers) 7)

(check-equal? (current-has-next? numbers) #f)
(check-equal? (current-has-previous? numbers) #t)

(set-current-to-previous! numbers)

(check-equal? (peek numbers) 6)

(set-current-to-first! numbers)

(delete! numbers)

(set-current-to-first! numbers)

(check-equal? (peek numbers) 2)

(set-current-to-last! numbers)

(delete! numbers)

(set-current-to-last! numbers)

(check-equal? (peek numbers) 6)

(find! numbers 4)

(delete! numbers)

(find! numbers 3)
(set-current-to-next! numbers)

(check-equal? (peek numbers) 5)

(set-current-to-previous! numbers)

(check-equal? (peek numbers) 3)

(add-before! numbers 13)

(set-current-to-first! numbers)

(add-before! numbers -1)

(set-current-to-last! numbers)

(add-after! numbers 100)

(find! numbers 13)

(update! numbers 555)