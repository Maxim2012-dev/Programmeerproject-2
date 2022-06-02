#lang r6rs


(import (prefix (a-d disk disk) disk:)
        (a-d file constants)
        (srfi :9)
        (prefix (a-d disk file-system) fs:)
        (prefix (a-d disk disk) dsk:)
        (prefix (a-d db table fixed-size-slots table) tbl:)
        (prefix (a-d db database) db:)
        (rnrs base)
        (rnrs io simple))


;; Disk + Database creation
(define disk (dsk:new "Hard-Disk"))
(fs:format! disk)
(define database (db:new disk "Zonnestelsel"))

;; =============== Tabel met manen ===============
(define manenschema  '((string 9) 
                       (string 9)  
                       (natural 2) 
                       (natural 2)  
                       (string 10)))

(define :maan-naam:       0)
(define :planeet:         1)
(define :maan-middellijn: 2)
(define :ontdekjaar:      3)
(define :ontdekker:       4)

(define manen (db:create-table database "Manen" manenschema))


;; =============== Tabel met planeten ===============
(define planetenschema '((string 9) 
                         (decimal) 
                         (decimal)  
                         (natural 3)
                         (decimal) 
                         (decimal)))

(define :planeet-naam:    0)
(define :afstand-tot-zon: 1)
(define :aard-massa:      2)
(define :middellijn:      3)
(define :omlooptijd:      4)
(define :rotatietijd:     5)

;; Create table + index
(define planeten (db:create-table database "Planeten" planetenschema))
(db:create-index! database planeten "Middellijn-zoeker" :middellijn:)


;; Population of database
(db:insert-into-table! database planeten 
                       (list "Mercurius" 0.3871   0.053   4840   0.241  +58.79))
(db:insert-into-table! database planeten 
                       (list "Venus"     0.7233   0.815  12200   0.615 -243.68))
(db:insert-into-table! database planeten 
                       (list "Aarde"     1.0000   1.000  12756   1.000   +1.00))
(db:insert-into-table! database planeten 
                       (list "Mars"      1.5237   0.109   6790   1.881   +1.03))
(db:insert-into-table! database planeten 
                       (list "Jupiter"   5.2028 317.900 142800  11.862   +0.41))
(db:insert-into-table! database planeten 
                       (list "Saturnus"  9.5388  95.100 119300  29.458   +0.43))
(db:insert-into-table! database planeten 
                       (list "Uranus"   19.1819  14.500  47100  84.013   -0.45))
(db:insert-into-table! database planeten 
                       (list "Neptunus" 30.0578  17.500  44800 164.793   +0.63))
(db:insert-into-table! database planeten 
                       (list "Pluto"    39.2975   1.000   5000 248.430   +0.26))


;; ============================ Test 1 ============================
(display "=================================================")(newline)
(display "Deletion of :middellijn: ----> index: 3")(newline)
(display "=================================================")(newline)
(db:alter-table-drop-column! database planeten :middellijn:)
(db:alter-table-drop-column! database planeten :aard-massa:)
(tbl:print planeten)
;(db:print database)
(newline)(newline)(newline)

