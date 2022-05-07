#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                  Coroutine Implementation Tests                 *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (rnrs base)
        (rnrs io simple)
        (rnrs control)
        (a-d examples coroutines))

(define p1
  (new-process
   (lambda (input)
     (do ()
       (#f)
       (display "Tick 1 ")
       (display input)
       (newline)
       (set! input (transfer p2 (+ input 1)))))))

(define p2
  (new-process
   (lambda (input)
     (do ()
       (#f)
       (display "Tick 2 ")
       (display input)
       (newline)
       (set! input (transfer p3 (+ input 1)))))))

(define p3
  (new-process
   (lambda (input)
     (do ()
       (#f)
       (display "Tick 3 ")
       (display input)
       (newline)
       (set! input (transfer p1 (+ input 1)))))))
