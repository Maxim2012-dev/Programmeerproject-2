#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*          Maze Application (Disjoint-Sets Illustration)          *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2012 Software Languages Lab                   *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (maze-application)
 (export print generate! new exit)
 (import (rnrs base)
         (srfi :9)
         (rnrs mutable-pairs)
         (rnrs bytevectors)
         (rnrs arithmetic bitwise)
         (rnrs control)
         (rnrs io simple)
         (prefix 
          (a-d disjoint-sets naive)
          dset:)
         (a-d scheme-tools))
 
 (define-record-type maze
   (make d e s)
   maze?
   (d dim)
   (e exit exit!)
   (s storage))

 (define (new size)
   (make size
         size       
         (make-bytevector (ceiling (/ (* 2 size size) 8)))))
 
 (define (h-bit-nr size row col)
   (+ (* col 2) (* 2 size row)))
 (define (v-bit-nr size row col)
   (+ 1 (h-bit-nr size row col)))
 (define (bit-set! bytes bit-nr)
   (define byte (bytevector-u8-ref bytes (div bit-nr 8)))
   (define bit (- 7 (mod bit-nr 8)))
   (bytevector-u8-set! bytes (div bit-nr 8) (bitwise-ior byte (expt 2 bit))))
 (define (bit-clear! bytes bit-nr)
   (define byte (bytevector-u8-ref bytes (div bit-nr 8)))
   (define bit (- 7 (mod bit-nr 8)))
   (bytevector-u8-set! bytes (div bit-nr 8) (bitwise-and byte (- 255 (expt 2 bit)))))
 (define (bit-get bytes bit-nr)
   (define byte (bytevector-u8-ref bytes (div bit-nr 8)))
   (define bit (- 7 (mod bit-nr 8)))
   (bitwise-and byte (expt 2 bit)))
 (define (bit-set? bytes bit-nr)
   (> (bit-get bytes bit-nr) 0))
 
 (define (all-walls-up! maze)
   (define bytes (storage maze))
   (bytevector-u8-map! bytes (lambda (idx val) 255)))
 (define (knock-down-vertical-wall! maze row col)
   (define bytes (storage maze))
   (define size (dim maze))
   (bit-clear! bytes (v-bit-nr size row col)))
 (define (knock-down-horizontal-wall! maze row col)
   (define bytes (storage maze))
   (define size (dim maze))
   (bit-clear! bytes (h-bit-nr size row col)))
 (define (test-vertical-wall? maze row col)
   (define bytes (storage maze))
   (define size (dim maze))
   (bit-set? bytes (v-bit-nr size row col)))
 (define (test-horizontal-wall? maze row col)
   (define bytes (storage maze))
   (define size (dim maze))
   (bit-set? bytes (h-bit-nr size row col)))
 
 (define (print maze)
   (define size (dim maze))
   (define way-out (exit maze))
   (display "   ")
   (do ((row 1 (+ row 1)))
     ((= (+ row 1) size) (display "_")(newline))
     (display "__"))
   (do ((row 0 (+ row 1)))
     ((= row size))
     (display "|")
     (do ((col 0 (+ col 1)))
       ((= col (- size 1))
        (if (test-horizontal-wall? maze row col)
            (if (= way-out row) (display "_ ") (display "_|"))
            (if (= way-out row) (display "  ") (display " |"))))
       (if (test-vertical-wall? maze row col)
           (if (test-horizontal-wall? maze row col)
               (display "_|")
               (display " |"))
           (if (test-horizontal-wall? maze row col)
               (display "__")
               (display "  "))))
     (newline)))
 
 (define (generate! maze)
   (define size (dim maze))
   (define exit-row (random-integer size))
   (define start-square 0)
   (define exit-square (- (* (+ exit-row 1) size) 1))
   (define paths (dset:new (* size size)))
   (exit! maze exit-row)
   (all-walls-up! maze)
   (do ((row (random-integer size) (random-integer size))
        (col (random-integer (- size 1)) (random-integer (- size 1))))
     ((dset:same-set? (dset:find paths start-square)
                      (dset:find paths exit-square)))
     (let ((square (+ (* row size) col)))
       (let ((square-set (dset:find paths square))
             (right-set (dset:find paths (+ square 1))))
         (when (not (dset:same-set? square-set right-set))
           (dset:union! paths square-set right-set)
           (knock-down-vertical-wall! maze row col)))
       (if (< row (- size 1))
           (let ((square-set (dset:find paths square))
                 (down-set (dset:find paths (+ square size))))
             (when (not (dset:same-set? square-set down-set))
               (dset:union! paths square-set down-set)
               (knock-down-horizontal-wall! maze row col))))))))