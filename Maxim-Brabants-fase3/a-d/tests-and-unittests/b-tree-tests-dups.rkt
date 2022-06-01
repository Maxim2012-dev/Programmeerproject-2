#!r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                   Tests for B-Trees with Duplicates             *-*-
;-*-*                                                                 *-*-
;-*-*                           Theo D'Hondt                          *-*-
;-*-*                 2010 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (rnrs base)
        (rnrs control)
        (rnrs io simple)
        (a-d file constants)
        (prefix (a-d db rcid) rcid:)
        (prefix (a-d db index b-tree b-tree-with-duplicates) b-tree:)
        (prefix (a-d disk file-system) fs:)
        (prefix (a-d disk disk) disk:))

(define dsk (disk:new "treedisk"))
(fs:format! dsk)
(define disk-size (fs:df dsk))
(define d (b-tree:new dsk "Manen" string-tag 10))

(b-tree:insert! d "Maan"      (rcid:new 1 1))
(b-tree:insert! d "Phobos"    (rcid:new 1 1))
(b-tree:insert! d "Phobos"    (rcid:new 2 1))
(b-tree:insert! d "Deimos"    (rcid:new 1 1))
(b-tree:insert! d "Io"        (rcid:new 1 1))
(b-tree:insert! d "Io"        (rcid:new 2 1))
(b-tree:insert! d "Io"        (rcid:new 3 1))
(b-tree:insert! d "Europa"    (rcid:new 1 1))
(b-tree:insert! d "Ganymedes" (rcid:new 1 1))
(b-tree:insert! d "Ganymedes" (rcid:new 2 1))
(b-tree:insert! d "Ganymedes" (rcid:new 3 1))
(b-tree:insert! d "Ganymedes" (rcid:new 4 1))
(b-tree:insert! d "Callisto"  (rcid:new 1 1))
(b-tree:insert! d "Mimas"     (rcid:new 1 1))
(b-tree:insert! d "Mimas"     (rcid:new 2 1))
(b-tree:insert! d "Mimas"     (rcid:new 3 1))
(b-tree:insert! d "Mimas"     (rcid:new 4 1))
(b-tree:insert! d "Mimas"     (rcid:new 5 1))
(b-tree:insert! d "Enceladus" (rcid:new 1 1))
(b-tree:insert! d "Tethys"    (rcid:new 1 1))
(b-tree:insert! d "Tethys"    (rcid:new 2 1))
(b-tree:insert! d "Tethys"    (rcid:new 3 1))
(b-tree:insert! d "Tethys"    (rcid:new 4 1))
(b-tree:insert! d "Tethys"    (rcid:new 5 1))
(b-tree:insert! d "Tethys"    (rcid:new 6 1))
(b-tree:insert! d "Dione"     (rcid:new 0 1))
(b-tree:insert! d "Rhea"      (rcid:new 0 1))
(b-tree:insert! d "Titan"     (rcid:new 0 1))
(b-tree:insert! d "Titan"     (rcid:new 0 1))
(b-tree:insert! d "Titan"     (rcid:new 0 1))
(b-tree:insert! d "Titan"     (rcid:new 0 1))
(b-tree:insert! d "Titan"     (rcid:new 0 1))
(b-tree:insert! d "Titan"     (rcid:new 0 1))
(b-tree:insert! d "Titan"     (rcid:new 0 1))
(b-tree:insert! d "Titan"     (rcid:new 0 1))
(b-tree:insert! d "Hyperion"  (rcid:new 0 1))
(b-tree:insert! d "Japetus"   (rcid:new 1 1))
(b-tree:insert! d "Japetus"   (rcid:new 2 1))
(b-tree:insert! d "Japetus"   (rcid:new 3 1))
(b-tree:insert! d "Japetus"   (rcid:new 4 1))
(b-tree:insert! d "Japetus"   (rcid:new 5 1))
(b-tree:insert! d "Japetus"   (rcid:new 6 1))
(b-tree:insert! d "Japetus"   (rcid:new 7 1))
(b-tree:insert! d "Japetus"   (rcid:new 8 1))
(b-tree:insert! d "Japetus"   (rcid:new 9 1))
(b-tree:insert! d "Phoebe"    (rcid:new 0 1))
(b-tree:insert! d "Janus"     (rcid:new 0 1))
(b-tree:insert! d "Ariel"     (rcid:new 0 1))
(b-tree:insert! d "Umbriel"   (rcid:new 0 1))
(b-tree:insert! d "Titania"   (rcid:new 0 1))
(b-tree:insert! d "Oberon"    (rcid:new 2 1))
(b-tree:insert! d "Miranda"   (rcid:new 0 1))
(b-tree:insert! d "Triton"    (rcid:new 0 1))
(b-tree:insert! d "Nereide"   (rcid:new 0 1))
(b-tree:insert! d "Nereide"   (rcid:new 0 1))
(b-tree:insert! d "Nereide"   (rcid:new 0 1))
(b-tree:insert! d "Nereide"   (rcid:new 0 1))
(b-tree:insert! d "Nereide"   (rcid:new 0 1))
(b-tree:insert! d "Nereide"   (rcid:new 0 1))
(b-tree:insert! d "Nereide"   (rcid:new 0 1))
(b-tree:print d)

(b-tree:find! d "Japetus")

(display (b-tree:peek d))(newline)

(b-tree:drop! d)

;(print-file d)
(display "DONE")
(if (not (= disk-size (fs:df dsk)))
    (error "Memory leackage!" disk-size (fs:df dsk)))

