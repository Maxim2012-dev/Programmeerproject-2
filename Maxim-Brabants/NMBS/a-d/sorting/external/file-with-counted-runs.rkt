#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                     File with Counted Runs                      *-*-
;-*-*                                                                 *-*-
;-*-*                        Wolfgang De Meuter                       *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (file-with-counted-runs)
 (export delete! make file file! 
         records-gone records-gone! 
         run-length run-length! name)
 (import (rnrs base)
         (srfi :9)
         (prefix (a-d file sequential sequential-file) seq:))

 (define-record-type file-with-runs
   (make-fwrs f g l)
   file-with-runs?
   (f file file!)
   (g records-gone records-gone!)
   (l run-length run-length!))
 (define file-with-runs-tag 'file-with-runs)
 
 (define (make file run-length)
   (make-fwrs file 0 run-length))

 (define (delete! fwrs)
   (seq:delete! (file fwrs)))
 
 (define (name fwrs)
   (seq:name (file fwrs))))