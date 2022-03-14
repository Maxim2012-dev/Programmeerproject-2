#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                            Disk Tests                           *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (rnrs base)
        (rnrs io simple)
        (rnrs control)
        (prefix (a-d disk config) disk:)
        (a-d disk file-system))

(define dsk (disk:new "d"))
(format! dsk)
(define d dsk)
(display (df dsk))(newline)
(define file1 (new-block dsk)) ; allocate 2 blocks (= test freelist archictecture + block management)
(define file2 (new-block dsk))
(display (df dsk))(newline)
(delete-block file1)
(delete-block file2)
(display (df dsk))(newline)
(display "ready for the fun!")(newline)
(define (fst)
  (mk dsk "rood" 9)
  (mk dsk "groen" 12)
  (mk dsk "blauw" 10)
  (mk dsk "appelblauwzeegroen" 11)
  (mk dsk "grasroodsocialistengroen" 12))
(define (snd)
  (mk dsk "cdenv" 43)
  (mk dsk "spa" 55)
  (mk dsk "vld" 19)
  (mk dsk "vlaams behang" 90)
  (mk dsk "groenuitroepteken" 99))
(define (thrd)
  (mk dsk "maandag" 43)
  (mk dsk "dinsdag" 99)
  (mk dsk "woensdag" 100)
  (mk dsk "donderdag" 111)
  (mk dsk "vrijdag" 12))

(define (fill)
  (fst)(snd)(thrd)
  
  (newline)
  (display (list "ls1: " (ls dsk)))(newline)
  (newline)
  (display (list "groenuitroepteken@" "is gevonden op " (whereis dsk "groenuitroepteken")))(newline)
  (newline))

(define (pluk)
  (rm dsk "rood")
  (rm dsk "vlaams behang")
  (rm dsk "woensdag"))
(define (strooi)
  (mk dsk "vodka" 99)
  (mk dsk "gin" 88)
  (mk dsk "whisky" 77))
(define (kiezing)
  (rm dsk "cdenv")
  (rm dsk "spa")
  (rm dsk "vld")
  (rm dsk "gin")
  (rm dsk "groenuitroepers"))

(define (empty)
  (pluk)
  (display (list "ls2: " (ls dsk)))(newline)
  (strooi)
  (display "=====")(newline)
  (display (list "ls3: " (ls dsk)))(newline)
  (kiezing)
  (display "-----")(newline)
  (display (list "ls4: " (ls dsk)))(newline))
