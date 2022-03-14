#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*              Coroutines Implementation (2nd Version)            *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2010 Software Languages Lab                   *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (coroutines2)
 (export new-process suspend start)
 (import (rnrs base)
         (prefix (a-d ring) ring:)
         (rnrs mutable-pairs))

  (define *ring-of-processes*
   (ring:new))
  
  (define (start value)
    ((cdr (ring:peek *ring-of-processes*)) value))
  
  (define (new-process procedure)
    (let ((process (cons 'proc procedure)))
      (ring:add-after! *ring-of-processes* process)
      process))
  
  (define (suspend value)
    (define *me* (ring:peek *ring-of-processes*))
    (call-with-current-continuation
     (lambda (c)
       (set-cdr! *me* c)
       (ring:shift-forward! *ring-of-processes*)
       (let ((target (cdr (ring:peek *ring-of-processes*))))
         (target value))))))

