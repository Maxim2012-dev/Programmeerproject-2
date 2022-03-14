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
 
 (define (make ptr)
   (cons handle-tag ptr))
 
 (define (handle-idx handle)
   (cdr handle))
 
 (define (pointer hidx)
   (scheme:vector-ref handle-mem hidx))
 
 (define (pointer! hidx ptr)
   (scheme:vector-set! handle-mem hidx ptr))
 
 (define (vector handle)
   (if (vector? handle)
     (pointer (handle-idx handle))
     (error "invalid handle" handle)))
 
 (define (initialize-handles)
   (do ((hidx 1 (+ hidx 1)))
     ((= hidx handle-mem-size))
     (pointer! (- hidx 1) hidx)))
 
 (define (crunch-vectors)
   (define (move-vector source destination size)
     (define hidx (vector:peek (+ source 1)))
     (vector:poke! destination size)
     (vector:poke! (+ destination 1) (pointer hidx))
     (pointer! hidx (vector:make destination))
     (do ((index 2 (+ index 1)))
       ((= index size))
       (vector:poke! (+ destination index) 
                     (vector:peek (+ source index)))))
   (define next-free
     (let move-left
       ((source 0)
        (destination 0))
       (if (>= source (vector:memory-size)) 
         destination
         (let ((size (vector:peek source)))
           (cond
             ((negative? size)
              (move-vector source destination (- size))
              (move-left (- source size) (- destination size)))
             (else
              (move-left (+ source size) destination)))))))
   (define size (- vector:memory-size next-free))
   (vector:reset-free)
   (if (>= size vector:lowest-size)
     (vector:insert-free next-free size)))
 
 (define (swap-pointer hidx)
   (define addr (vector:address (pointer hidx)))
   (define size (vector:peek addr))
   (pointer! hidx (vector:peek (+ addr 1)))
   (vector:poke! addr (- size))
   (vector:poke! (+ addr 1) handle-idx))
 
 (define (defragment-storage)
   (do ((hidx 0 (+ hidx 1)))
     ((= hidx handle-mem-size))
     (if (vector:vector? (pointer hidx))
       (swap-pointer hidx)))
   (crunch-vectors))
 
 (define (make-vector size)
   (if (eq? free-handle vector:null)
     (error "handle table overflow" size)
     (let
         ((hidx free-handle)
          (vctr (vector:make-vector size)))
       (when (eq? vctr vector:null)
         (defragment-storage)
         (set! vctr (vector:make-vector size))
         (if (eq? vctr vector:null)
           (error "Memory overflow" size)))
       (set! free-handle (pointer free-handle))
       (pointer! hidx vctr)
       (make hidx))))
 
 (define (vector-free vctr)
   (vector:vector-free (vector vctr))
   (let 
       ((hidx (handle-idx vctr)))
     (pointer! hidx free-handle)
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