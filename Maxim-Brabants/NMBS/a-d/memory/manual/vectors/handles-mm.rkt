#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*            Best-Fit Memory Manager (Handle Management)          *-*-
;-*-*                                                                 *-*-
;-*-*                 Theo D'Hondt - Wolfgang De Meuter               *-*-
;-*-*               1993-2009 Programming Technology Lab              *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (handle-manager)
 (export make-vector vector? vector-free vector-ref vector-set! vector-length)
 (import (rename (except (rnrs base) vector vector? vector-length)
                 (make-vector scheme:make-vector) (vector-ref scheme:vector-ref) (vector-set! scheme:vector-set!))
         (rnrs control)
         (prefix (a-d memory manual vectors best-fit-mm) vector:))
 
 (define free-handle 0)
 (define handle-mem-size 16)
 (define handle-mem (scheme:make-vector handle-mem-size vector:null))
 
 (define handle-tag 'handle)
 
 (define (tag ptr)
   (cons handle-tag ptr))
 
 (define (untag handle)
   (cdr handle))
 
 (define (get-pointer hidx)
   (scheme:vector-ref handle-mem hidx))
 
 (define (set-pointer! hidx ptr)
   (scheme:vector-set! handle-mem hidx ptr))
 
 (define (vector handle)
   (if (vector? handle)
     (get-pointer (untag handle))
     (error "invalid handle" handle)))
 
 (define (initialize-handles)
   (do ((hidx 1 (+ hidx 1)))
     ((= hidx handle-mem-size))
     (set-pointer! (- hidx 1) hidx)))
 
 (define (crunch-vectors)
   (define (move-vector source destination size)
     (define hidx (vector:peek (+ source 1)))
     (vector:poke! destination size)
     (vector:poke! (+ destination 1) (get-pointer hidx))
     (set-pointer! hidx (vector:tag destination))
     (do ((index 2 (+ index 1)))
       ((= index size))
       (vector:poke! (+ destination index) 
                     (vector:peek (+ source index)))))
   (define next-free
     (let move-left
       ((source 0)
        (destination 0))
       (if (>= source vector:memory-size) 
         destination
         (let ((size (vector:peek source)))
           (cond
             ((negative? size)
              (move-vector source destination (- size))
              (move-left (- source size) (- destination size)))
             (else
              (move-left (+ source size) destination)))))))
   (define size (- vector:memory-size next-free))
   (vector:reset-trees)
   (if (>= size vector:smallest-size)
     (vector:insert-free next-free size)))
 
 (define (swap-pointer hidx)
   (define addr (vector:untag (get-pointer hidx)))
   (define size (vector:peek addr))
   (set-pointer! hidx (vector:peek (+ addr 1)))
   (vector:poke! addr (- size))
   (vector:poke! (+ addr 1) hidx))
 
 (define (defragment-storage)
   (do ((hidx 0 (+ hidx 1)))
     ((= hidx handle-mem-size))
     (if (vector:vector? (get-pointer hidx))
       (swap-pointer hidx)))
   (crunch-vectors))
 
 (define (make-vector size)
   (if (null? free-handle)
     (error "handle table overflow" size)
     (let
         ((hidx free-handle)
          (vctr (vector:make-vector size)))
       (when (null? vctr)
         (defragment-storage)
         (set! vctr (vector:make-vector size))
         (if (null? vctr)
           (error "Memory overflow" size)))
       (set! free-handle (get-pointer free-handle))
       (set-pointer! hidx vctr)
       (tag hidx))))
 
 (define (vector-free vctr)
   (vector:vector-free (vector vctr))
   (let 
       ((hidx (untag vctr)))
     (set-pointer! hidx free-handle)
     (set! free-handle hidx)))
 
 (define (vector? any)
   (and (pair? any)
        (eq? (car any) handle-tag)))
 
 (define (vector-ref vctr index)
   (vector:vector-ref (vector vctr) index))
 
 (define (vector-set! vctr index value)
   (vector:vector-set! (vector vctr) index value))
 
 (define (vector-length vctr)
   (vector:vector-length (vector vctr)))
 
 (initialize-handles))