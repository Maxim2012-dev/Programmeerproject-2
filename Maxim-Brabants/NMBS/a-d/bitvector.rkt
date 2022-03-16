#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                           Bit Vectors                           *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
(library
 (bit-vector)
 (export make-bitvector bitvector-ref bitvector-set! bitvector-length)
 (import (rnrs base)
         (srfi :9)
         (rnrs control)
         (rnrs arithmetic bitwise)
         (rnrs bytevectors))
 
 (define-record-type bitvector
   (make n b)
   bitvector?
   (n size)
   (b bytes))
 
 (define (make-bitvector n)
   (define extra (if (> (mod n 8) 0) 1 0))
   (make n (make-bytevector (+ (div n 8) extra))))
 
 (define (bitvector-set! v i b)
   (define bits (bytes v))
   (define byte (bytevector-u8-ref bits (div i 8)))
   (define bit (- 7 (mod i 8)))
   (bytevector-u8-set! bits (div i 8) 
                       (if b 
                           (bitwise-ior byte (expt 2 bit))
                           (bitwise-and byte (- 255 (expt 2 bit))))))
 
 (define (bitvector-ref v i)
   (define bits (bytes v))
   (define byte (bytevector-u8-ref bits (div i 8)))
   (define bit (- 7 (mod i 8)))
   (< 0 (bitwise-and byte (expt 2 bit))))
 
 (define (bitvector->vector v)
   (define ts (make-vector (size v) #f))
   (do ((i 0 (+ i 1)))
     ((= i (size v)) ts)
     (vector-set! ts i (bitvector-ref v i))))
 
 (define (bitvector-length v)
   (size v)))