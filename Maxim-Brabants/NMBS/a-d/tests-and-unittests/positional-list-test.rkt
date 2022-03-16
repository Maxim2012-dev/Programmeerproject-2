#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                      Positional List Tests                      *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import
 (a-d positional-list adt)
 (except (rnrs base) map list? length for-each)
 (only (scheme base) require planet)
 (rnrs io simple))

(require (planet schematics/schemeunit:3/test))

(define names-list (new string=?))

(check-equal? (empty? names-list) #t)

(add-after! names-list "viviane")
(check-equal? (empty? names-list) #f)
(add-after! names-list "wolfgang")
(add-before! names-list "theo")

(define first1 (first names-list))
(define first2 (find names-list "theo"))

(check-equal? first1 first2)

(define last1 (last names-list))
(define last2 (find names-list "wolfgang"))

(check-equal? (peek names-list last2) "wolfgang")

(update! names-list last2 "el lobo")
(define last3 (find names-list "el lobo"))

(check-equal? last1 last2)
(check-equal? last2 last3)
(check-equal? (has-next? names-list first1) #t)
(check-equal? (has-next? names-list last1) #f)
(check-equal? (has-previous? names-list first1) #f)
(check-equal? (has-previous? names-list last1) #t)

(define middle-position1 (next names-list first1))
(define middle-position2 (previous names-list last1))

(check-equal? middle-position1 middle-position2)

(add-before! names-list "works with" middle-position1)
(set! middle-position2 (find names-list "viviane"))
(add-after! names-list "and works with" middle-position2)

(define inserted1 (find names-list "works with"))
(define inserted2 (find names-list "and works with"))
(set! middle-position1 (find names-list "viviane"))
(set! first1 (first names-list))
(set! last1 (last names-list))
(check-equal? (length names-list) 5)

(define names-list-copy (map names-list (lambda (x) (string-length x)) =))

(check-equal? (next names-list inserted1)  middle-position1)
(check-equal? (previous names-list inserted1) first1)
(check-equal? (next names-list inserted2) last1)
(check-equal? (previous names-list inserted2) middle-position1)

(delete! names-list middle-position1)
(set! inserted1 (find names-list "works with"))
(set! inserted2 (find names-list "and works with"))

(check-equal? (next names-list inserted1) inserted2)
(check-equal? (previous names-list inserted2) inserted1)

(delete! names-list inserted1)
(set! inserted2 (find names-list "and works with"))
(delete! names-list inserted2)

(check-equal? (length names-list) 2)
(check-equal? (first names-list) (previous names-list (last names-list)))
(check-equal? (next names-list (first names-list)) (last names-list))

(delete! names-list (last names-list))

(check-equal? (has-next? names-list (first names-list)) #f)
(check-equal? (has-previous? names-list (first names-list)) #f)
(check-equal? (has-next? names-list (last names-list)) #f)
(check-equal? (has-previous? names-list (last names-list)) #f)

(delete! names-list (first names-list))

(check-equal? (empty? names-list) #t)

(define names-list1 '("theo" "->" "viviane" "-->" "wolf"))
(set! names-list (from-scheme-list names-list1 string=?))
(display names-list1)(newline)
