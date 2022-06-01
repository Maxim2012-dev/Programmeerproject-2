#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                        DIMACS CNF Parser                        *-*-
;-*-*                                                                 *-*-
;-*-*                           Youri Coppens                         *-*-
;-*-*                           Bart Bogaerts                         *-*-
;-*-*                 2021 Artificial Intelligence Lab                *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

; Inspired on the Racket script made by Guannan Wei

(library
 (dimacs-parser)
 (export parse-dimacs-file)
 (import (rnrs base)
         (rnrs io ports)
         (rnrs io simple)
         (rnrs lists)
         (only (racket string) non-empty-string? string-split)
         (only (racket mpair) list->mlist) ; string-split returns a Racket list which is incompatible with R6RS lists
         (a-d sat cnf)) 
 
 (define (all-but-last l)
   (cond ((null? l) (assertion-violation 'all-but-last "empty list"))
         ((null? (cdr l)) '())
         (else (cons (car l) (all-but-last (cdr l))))))

 (define (read-lines port)
   (if (textual-port? port)
       (let loop ()
         (if (port-eof? port)
             '()
             (cons (get-line port) (loop))))
       (assertion-violation 'read-lines "non-textual port")))

 (define (parse in-port)
   (define var-ctr #f)
   (define clause-ctr #f)

   (define (parse-literal str)
     (let ((n (string->number str)))
       (make-literal (abs n) (< n 0))))

   (define (parse-clause line)
     (let ((lits (map parse-literal (all-but-last (list->mlist (string-split line))))))
       (make-clause lits (length lits))))
   
   (define (parse-header line)
     (if (char=? (string-ref line 0) #\p)
         (let ((words (list->mlist (string-split line))))
           (if (and (= (length words) 4)
                    (string=? (list-ref words 1) "cnf"))
               (begin (set! var-ctr (string->number (list-ref words 2)))
                      (set! clause-ctr (string->number (list-ref words 3))))
               (assertion-violation 'parse-header "incorrect header line format" line)))
         (assertion-violation 'parse-header "line doesn't start with char 'p'" line)))
   
   ; fold-left with append is rather inperformant
   ; a fold-right with cons would seem the way to go, but this doesn't give the desired result... 
   (let ((clauses (fold-left
                   (lambda (acc line) 
                     (if (non-empty-string? line)
                         (cond ((char=? (string-ref line 0) #\c) acc) ; skip irrelevant lines
                               ((char=? (string-ref line 0) #\%) acc)
                               ((char=? (string-ref line 0) #\#) acc)
                               ((char=? (string-ref line 0) #\0) acc)
                               ((char=? (string-ref line 0) #\p) (parse-header line) acc)
                               (else (append acc (list (parse-clause line)))))
                         acc))
                   '()
                   (read-lines in-port))))
     (if (and var-ctr
              (= (length clauses) clause-ctr))
         (make-formula clauses var-ctr clause-ctr)
         (error 'parse "something went wrong during parsing" var-ctr clause-ctr (length clauses)))))

 (define (parse-dimacs-file filename)
   (call-with-input-file filename parse))
 )