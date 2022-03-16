#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                     Relational Tables Test                      *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (prefix (a-d disk disk) disk:)
        (prefix (a-d disk file-system) unix:)
        (prefix (a-d db table fixed-size-slots table) tbl:)
        (prefix (a-d file sequential input-file) seq:)
        (a-d file constants)
        (rnrs base)
        (rnrs control)
        (rnrs io simple)
        (a-d scheme-tools))
 

(define d (disk:new "testdisk"))
(unix:format! d)
(define disk-size (unix:df d))

(display (list "disk size =" (unix:df d)))
(newline)

(define naam 0)
(define afstand-tot-zon 1)
(define aard-massa 2)
(define middellijn 3)
(define omlooptijd 4)
(define rotatietijd 5)
(define planeten (tbl:new d "planeten" '((string 9) ; naam
                                         (decimal)  ; afstand tot de zon
                                         (decimal)  ; aard-massa
                                         (natural 3); middellijn
                                         (decimal)  ; omlooptijd aardjaar
                                         (decimal)  ; rotatietijd aarddag
                                         )))
(tbl:insert! planeten (list "Mercurius" 0.3871   0.053   4840   0.241  +58.79))
(tbl:insert! planeten (list "Venus"     0.7233   0.815  12200   0.615 -243.68))
(tbl:insert! planeten (list "Aarde"     1.0000   1.000  12756   1.000   +1.00))
(tbl:insert! planeten (list "Mars"      1.5237   0.109   6790   1.881   +1.03))
(tbl:insert! planeten (list "Jupiter"   5.2028 317.900 142800  11.862   +0.41))
(tbl:insert! planeten (list "Saturnus"  9.5388  95.100 119300  29.458   +0.43))
(tbl:insert! planeten (list "Uranus"   19.1819  14.500  47100  84.013   -0.45))
(tbl:insert! planeten (list "Neptunus" 30.0578  17.500  44800 164.793   +0.63))
(tbl:insert! planeten (list "Pluto"    39.2975   1.000   5000 248.430   +0.26))
(tbl:print planeten)
(tbl:close! planeten)

(define maan-naam 0)
(define planeet-naam 1)
(define maan-middellijn 2)
(define ontdekjaar 3)
(define ontdekker 4)
(define manen  (tbl:new d "manen" '((string 9)  ; naam maan
                                    (string 9)  ; naam planeet
                                    (natural 2) ; middellijn
                                    (natural 2) ; ontdekjaar
                                    (string 10) ; ontdekker
                                    )))
; test insert
(define manenvector1 (vector
                      (tbl:insert! manen (list "Maan"      "Aarde"    3476 1877 ""))
                      (tbl:insert! manen (list "Phobos"    "Mars"       22 1877 "Hall"))
                      (tbl:insert! manen (list "Deimos"    "Mars"        8 1610 "Hall"))
                      (tbl:insert! manen (list "Io"        "Jupiter"  3550 1610 "Galilei"))
                      (tbl:insert! manen (list "Europa"    "Jupiter"  3100 1610 "Galilei"))
                      (tbl:insert! manen (list "Ganymedes" "Jupiter"  5600 1610 "Galilei"))
                      (tbl:insert! manen (list "Callisto"  "Jupiter"  5050 1610 "Galilei"))
                      (tbl:insert! manen (list "Mimas"     "Saturnus"  520 1789 "Herschel"))
                      (tbl:insert! manen (list "Enceladus" "Saturnus"  600 1789 "Herschel"))
                      (tbl:insert! manen (list "Tethys"    "Saturnus" 1200 1684 "Cassini"))
                      (tbl:insert! manen (list "Dione"     "Saturnus" 1300 1684 "Cassini"))
                      (tbl:insert! manen (list "Rhea"      "Saturnus" 1300 1672 "Cassini"))))
; test flush
(tbl:close! manen)
; test re-read from disk
(set! manen (tbl:open d "manen"))
; continue inserting
(define manenvector2 (vector
                     (tbl:insert! manen (list "Titan"     "Saturnus" 4950 1655 "Huygens"))
                     (tbl:insert! manen (list "Hyperion"  "Saturnus"  400 1848 "Bond"))
                     (tbl:insert! manen (list "Japetus"   "Saturnus" 1200 1671 "Cassini"))
                     (tbl:insert! manen (list "Phoebe"    "Saturnus"  300 1898 "Pickering"))
                     (tbl:insert! manen (list "Janus"     "Saturnus"  350 1966 "Dolfus"))
                     (tbl:insert! manen (list "Ariel"     "Uranus"    600 1851 "Lassell"))
                     (tbl:insert! manen (list "Umbriel"   "Uranus"    400 1851 "Lassell"))
                     (tbl:insert! manen (list "Titania"   "Uranus"   1000 1787 "Herschel"))
                     (tbl:insert! manen (list "Oberon"    "Uranus"    800 1787 "Herschel"))
                     (tbl:insert! manen (list "Miranda"   "Uranus"    100 1948 "Kuiper"))
                     (tbl:insert! manen (list "Triton"    "Neptunus" 4000 1846 "Lassell"))
                     (tbl:insert! manen (list "Nereide"   "Neptunus"  300 1949 "Kuiper"))))
;test structure of the table
(tbl:print manen)
(tbl:print planeten)
; test current system (first+next)
(define (print-all t)
  (tbl:set-current-to-first! t)
  (let loop
    ((p (tbl:peek t)))
    (display p)(display " | ")
    (when (not (eq? (tbl:set-current-to-next! t) no-current))
      (loop (tbl:peek t)))))

(newline)
(display "DOORLOOP set-current-to-first")
(newline)
(print-all manen)
(newline)
; test find
(display "testing find")(newline)
(tbl:current! manen (vector-ref manenvector2 4))
(display (tbl:peek manen))(newline) ; should be (Janus....) see above
(display (list "^^^ this must be the Janus tuple"))(newline)

;testing deletion and destruction
(define manual-delete? #f)
(cond (manual-delete?
       (tbl:delete! manen (vector-ref manenvector2 11))
       (tbl:delete! manen (vector-ref manenvector2 0))
       (tbl:delete! manen (vector-ref manenvector2 5))
       (tbl:delete! manen (vector-ref manenvector2 4))
       (tbl:delete! manen (vector-ref manenvector2 9))
       (tbl:delete! manen (vector-ref manenvector2 6))
       (tbl:delete! manen (vector-ref manenvector2 1))
       (tbl:delete! manen (vector-ref manenvector2 2))
       (tbl:delete! manen (vector-ref manenvector2 3))
       (tbl:delete! manen (vector-ref manenvector2 7))
       (tbl:delete! manen (vector-ref manenvector2 8))
       (tbl:delete! manen (vector-ref manenvector2 10))
       (tbl:delete! manen (vector-ref manenvector1 11))
       (tbl:delete! manen (vector-ref manenvector1 0))
       (tbl:delete! manen (vector-ref manenvector1 5))
       (tbl:delete! manen (vector-ref manenvector1 4))
       (tbl:delete! manen (vector-ref manenvector1 9))
       (tbl:delete! manen (vector-ref manenvector1 6))
       (tbl:delete! manen (vector-ref manenvector1 1))
       (tbl:delete! manen (vector-ref manenvector1 2))
       (tbl:delete! manen (vector-ref manenvector1 3))
       (tbl:delete! manen (vector-ref manenvector1 7))
       (tbl:delete! manen (vector-ref manenvector1 8))
       (tbl:delete! manen (vector-ref manenvector1 10))
       (tbl:drop! manen)
       )
      (else
       (tbl:drop! manen)))

(tbl:drop! planeten)
(display "DONE")
(if (not (= disk-size (unix:df d)))
    (error "Memory leackage!" disk-size (unix:df d)))


;(tbl:create-material-table-scan-index planeten (lambda (tuple) #t) "alles-idx")
;
;(define (traverse-selected-idx proc)
;  (define f (seq:open-read! d "alles-idx"))
;  (define (read-one)
;    (define blk-nr (seq:read f))
;    (define blk-ix (seq:read f))
;    (proc blk-nr blk-ix)
;    (when (seq:has-more? f)
;      (read-one)))
;  (read-one)
;  (seq:close-read! f))
;
;(traverse-selected-idx
; (lambda (blk-nbr blk-idx)
;   (display (tbl:read-record planeten blk-nbr blk-idx))(newline)))
;
;(tbl:create-dense-theta-join-index planeten manen
;                                   (lambda (tup1 tup2)
;                                     (string=? (list-ref tup1 naam)
;                                               (list-ref tup2 planeet-naam)))
;                                   "join-idx")
;(define (traverse-joined-idx proc)
;  (define f (seq:open-read! d "join-idx"))
;  (define (read-one)
;    (define blk1-nr (seq:read f))
;    (define blk1-ix (seq:read f))
;    (define blk2-nr (seq:read f))
;    (define blk2-ix (seq:read f))
;    (proc blk1-nr blk1-ix blk2-nr blk2-ix)
;    (when (seq:has-more? f)
;      (read-one)))
;  (read-one)
;  (seq:close-read! f))
;
;(traverse-joined-idx
; (lambda (blk-nbr1 blk-idx1 blk-nbr2 blk-idx2)
;   (display (list (tbl:read-record planeten blk-nbr1 blk-idx1) " |><| " (tbl:read-record manen blk-nbr2 blk-idx2)))(newline)))
;
;(define idx1 (tbl:create-pipelined-table-scan-index manen (lambda (tuple) (> (string-length (list-ref tuple 0)) 5))))
;
;(define idx2 (tbl:create-pipelined-selection idx1 (lambda (tuple) (> (list-ref tuple 2) 500))))
;
;(tbl:create-material-projection manen (list maan-naam ontdekjaar ontdekker) "discovery")

;(tbl:print-table (tbl:open-table d "discovery"))
;
;(define idx3 (tbl:create-pipelined-projection idx2 (list maan-naam ontdekjaar ontdekker)))