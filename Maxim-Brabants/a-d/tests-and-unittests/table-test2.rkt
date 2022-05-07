#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                           Tables Test                           *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (prefix (a-d disk disk) disk:)
        (prefix (a-d disk file-system) fs:)
        (prefix (a-d db table fixed-size-slots table) tbl:)
        (prefix (a-d file sequential input-file) seq:)
        (rnrs base)
        (rnrs control)
        (rnrs io simple)
        (a-d scheme-tools))
 
(define d (disk:new "testdisk"))
(fs:format! d)

(display (list "disk size =" (fs:df d)))
(newline)

(define maan-naam 0)
(define ontdekjaar 1)
(define ontdekker 2)
(define manen  (tbl:new d "discovery" '((string 9)  ; naam maan
                                        (natural 2) ; ontdekjaar
                                        (string 10) ; ontdekker
                                        )))
(define maanvector
  (vector 
   (tbl:insert! manen (list "Phobos"    1877 "Hall"))
   (tbl:insert! manen (list "Maan"      1877 ""))
   (tbl:insert! manen (list "Deimos"    1610 "Hall"))
   (tbl:insert! manen (list "Io"        1610 "Galilei"))
   (tbl:insert! manen (list "Europa"    1610 "Galilei"))
   (tbl:insert! manen (list "Ganymedes" 1610 "Galilei"))
   (tbl:insert! manen (list "Callisto"  1610 "Galilei"))
   (tbl:insert! manen (list "Mimas"     1789 "Herschel"))
   (tbl:insert! manen (list "Enceladus" 1789 "Herschel"))
   (tbl:insert! manen (list "Tethys"    1684 "Cassini"))
   (tbl:insert! manen (list "Dione"     1684 "Cassini"))
   (tbl:insert! manen (list "Rhea"      1672 "Cassini"))
   (tbl:insert! manen (list "Titan"     1655 "Huygens"))
   (tbl:insert! manen (list "Hyperion"  1848 "Bond"))
   (tbl:insert! manen (list "Japetus"   1671 "Cassini"))
   (tbl:insert! manen (list "Phoebe"    1898 "Pickering"))
   (tbl:insert! manen (list "Janus"     1966 "Dolfus"))
   (tbl:insert! manen (list "Ariel"     1851 "Lassell"))
   (tbl:insert! manen (list "Umbriel"   1851 "Lassell"))
   (tbl:insert! manen (list "Titania"   1787 "Herschel"))
   (tbl:insert! manen (list "Oberon"    1787 "Herschel"))
   (tbl:insert! manen (list "Miranda"   1948 "Kuiper"))
   (tbl:insert! manen (list "Triton"    1846 "Lassell"))
   (tbl:insert! manen (list "Nereide"   1949 "Kuiper"))))
(tbl:print manen)
(tbl:close! manen)
(tbl:print (tbl:open d "discovery"))
(tbl:drop! manen)
(display (list "disk size =" (fs:df d)))
(newline)
