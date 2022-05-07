#!r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                      Labeled Graphs Tests                       *-*-
;-*-*                                                                 *-*-
;-*-*               Wolfgang De Meuter & Matthias Stevens             *-*-
;-*-*                  2009-10  Software Languages Lab                *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (a-d graph labeled config)
        (rnrs base)
        (rnrs io simple)
        (only (scheme base) require planet))

(require (planet schematics/schemeunit:3/test))

(define (run-tests directed)
  (define g (new directed 5))
  (define lbl03 'test)
  (define lbl34 'hello)
  (define lbl23 'world)
  (define lbldrie 'drie)
  
  (label! g 0 'nul)
  (label! g 1 'een)
  (label! g 2 'twee)
  (label! g 3 lbldrie)
  (label! g 4 'vier)
  
  (check-equal? (label g 3) lbldrie)
  
  (add-edge! g 0 3 lbl03)
  (check-equal? (adjacent? g 0 3) #t)
  (check-equal? (adjacent? g 3 0) (not directed))
  (check-equal? (edge-label g 0 3) lbl03)
  
  (delete-edge! g 0 3)
  (check-equal? (adjacent? g 0 3) #f)
  (check-equal? (adjacent? g 3 0) #f)
  (check-equal? (edge-label g 0 3) #f)
  
  (add-edge! g 3 4 lbl34)
  (check-equal? (edge-label g 3 4) lbl34)
  (check-equal? (edge-label g 4 3) (if directed
                                       #f
                                       lbl34))
  
  (add-edge! g 2 3 lbl23)
  (check-equal? (edge-label g 2 3) lbl23)
  
  (display "All nodes:\n")
  (for-each-node g (lambda (n label)
                     (display " - node ")(display n)(display ": ")(display label)(newline)))
  
  (display "All edges from node 3:\n")
  (for-each-edge g 3 (lambda (to edge-label)
                       (display " - to ")(display to)(display ": ")(display edge-label)(newline)))
  )

(display "Undirected tests...\n")
(run-tests #f)
(newline)

(display "Directed tests...\n")
(run-tests #t)
(newline)