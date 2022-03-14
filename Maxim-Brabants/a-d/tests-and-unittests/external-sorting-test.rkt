#!r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                      External Sorting Tests                     *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (rnrs base)
        (a-d scheme-tools)(rnrs io simple)
        (prefix (a-d file sequential) file:)
        (prefix (a-d sorting external two-way-merge-sort) twm:)
        (prefix (a-d sorting external n-way-merge-sort) nwm:))

 
 (define (generate! rnd size fname)
   (define out (file:new fname))
   (define (iter idx)
     (if (> idx 0)
       (begin
         (file:write-record! out (rnd))
         (iter (- idx 1)))
       (file:close! out)))
   (file:open-write! out)
   (iter size)
   out)
 
(define (gen m)
  (generate! (lambda () (random-inbetween 1000 9999)) 143 m))

(define (sor-n f)
  (define ff (gen f))
  (nwm:sort ff < +inf.0))
(define (sor-2 f)
  (define ff (gen f))
  (twm:sort ff < +inf.0))

(sor-2 "m")