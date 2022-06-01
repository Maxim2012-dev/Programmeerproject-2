#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*           Best-fit Memory Manager (Storage Management)          *-*-
;-*-*                                                                 *-*-
;-*-*                Theo D'Hondt - Wolfgang De Meuter                *-*-
;-*-*              1993-2009 Programming Technology Lab               *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (best-fit-mm)
 (export null make-vector vector? vector-length vector-ref vector-set! vector-free 
         peek poke! tag memory-size reset-trees smallest-size insert-free untag)
 (import  
  (rnrs control)
  (rename (except (rnrs base) vector? vector-length)
          (set! scheme:set!) (vector-set! scheme:vector-set!) (vector-ref scheme:vector-ref) (make-vector scheme:make-vector)))
 
 (define null ())
 (define smallest-size 5)
 
 (define memory-size 100)
 (define memory (scheme:make-vector memory-size null))
 
 (define (peek addr)
   (scheme:vector-ref memory addr))
 
 (define (poke! addr value)
   (scheme:vector-set! memory addr value))
 
 (define address-tree null)
 (define size-tree    null)
 
 (define (size addr)
   (peek (+ addr 0)))
 
 (define (size! addr siz)
   (poke! (+ addr 0) siz))
 
 (define (size-left addr)
   (peek (+ addr 1)))
 
 (define (size-left! addr size-left)
   (poke! (+ addr 1) size-left))
 
 (define (size-right addr)
   (peek (+ addr 2)))
 
 (define (size-right! addr size-right)
   (poke! (+ addr 2) size-right))
 
 (define (address-left addr)
   (peek (+ addr 3)))
 
 (define (address-left! addr addr-left)
   (poke! (+ addr 3) addr-left))
 
 (define (address-right addr)
   (peek (+ addr 4)))
 
 (define (address-right! addr addr-right)
   (poke! (+ addr 4) addr-right))
 
 (define (find-address addr)
   (let lookup
     ((curr address-tree))
     (cond 
       ((null? curr)
        ())
       ((= curr addr)
        curr)
       ((< addr curr)
        (lookup (address-left curr)))
       (else
        (let ((try (lookup (address-right curr))))
          (if (null? try)
            curr
            try))))))
 
 (define (find-size req-size) 
   (let lookup 
     ((curr size-tree))
     (cond 
       ((null? curr)
        ())
       ((= (size curr) req-size)
        curr)
       ((< (size curr) req-size)
        (lookup (size-right curr)))
       (else 
        (let ((try (lookup (size-left curr))))
          (if (null? try)
            curr
            try))))))
 
 (define (insert-free addr siz)
   (define (insert-address curr)
     (if (< curr addr)
       (let 
           ((right (address-right curr)))
         (if (null? right)
           (address-right! curr addr)
           (insert-address right)))
       (let 
           ((left (address-left curr)))
         (if (null? left)
           (address-left! curr addr)
           (insert-address left)))))
   (define (insert-size curr)
     (if (< siz (size curr))
       (let 
           ((left (size-left curr)))
         (if (null? left)
           (size-left! curr addr)
           (insert-size left)))
       (let 
           ((right (size-right curr)))
         (if (null? right)
           (size-right! curr addr)
           (insert-size right)))))
   (size! addr siz)
   (address-left!  addr null)
   (address-right! addr null)
   (size-left!     addr null)
   (size-right!    addr null)
   (if (null? address-tree)
     (scheme:set! address-tree addr)
     (insert-address address-tree))
   (if (null? size-tree)
     (scheme:set! size-tree addr)
     (insert-size size-tree)))
 
 (define (delete-free addr)
   (define siz (size addr))
   (define (delete-next-address curr op)
     (cond
       ((null? (address-right curr))
        (op (address-left curr))
        curr)
       (else
        (delete-next-address (address-right curr)
                             (lambda (ref) 
                               (address-right! curr ref))))))
   (define (delete-address curr op)
     (cond
       ((> addr curr)
        (delete-address (address-right curr)
                        (lambda (ref) 
                          (address-right! curr ref))))
       ((< addr curr)
        (delete-address (address-left curr) 
                        (lambda (ref) 
                          (address-left! curr ref))))
       ((null? (address-left curr))
        (op (address-right curr)))
       ((null? (address-right curr))
        (op (address-left curr)))
       (else
        (let
            ((hold (delete-next-address (address-left curr) 
                                        (lambda (ref) 
                                          (address-left! curr ref)))))
          (address-left! hold (address-left curr))
          (address-right! hold (address-right curr))
          (op hold)))))
   (define (delete-next-size curr op)
     (cond
       ((null? (size-left curr))
        (op (size-right curr))
        curr)
       (else
        (delete-next-size (size-left curr)
                          (lambda (ref) 
                            (size-left! curr ref))))))
   (define (delete-size curr op)
     (cond
       ((< siz (size curr))
        (delete-size (size-left curr)
                     (lambda (ref) 
                       (size-left! curr ref))))
       ((> siz (size curr))
        (delete-size (size-right curr) 
                     (lambda (ref) 
                       (size-right! curr ref))))
       ((not (= addr curr))
        (delete-size (size-right curr) 
                     (lambda (ref) 
                       (size-right! curr ref))))
       ((null? (size-left curr))
        (op (size-right curr)))
       ((null? (size-right curr))
        (op (size-left curr)))
       (else
        (let
            ((hold (delete-next-size (size-right curr) 
                                     (lambda (ref) 
                                       (size-right! curr ref)))))
          (size-left! hold (size-left curr))
          (size-right! hold (size-right curr))
          (op hold)))))
   (delete-address address-tree (lambda (ref)
                                  (scheme:set! address-tree ref)))
   (delete-size size-tree (lambda (ref)
                            (scheme:set! size-tree ref))))
 
 (define (reset-trees)
   (scheme:set! address-tree null)
   (scheme:set! size-tree   null))
 
 (define (initialize)
   (insert-free 0 memory-size))
 
 (define vector-tag 'vector) 
 
 (define (tag addr)
   (cons vector-tag addr))
 
 (define (untag vctr)
   (cdr vctr))
 
 (define (vector? any)
   (and (pair? any)
        (eq? (car any) vector-tag)))
 
 (define (make-vector size)
   (scheme:set! size (+ size 1))
   (if (< size smallest-size)
     (scheme:set! size smallest-size))
   (let
       ((found-addr (find-size size))) 
     (if (null? found-addr)
       null
       (let
           ((found-size (peek found-addr)))
         (delete-free found-addr)
         (cond
           ((< smallest-size (- found-size size))
            (insert-free (+ found-addr size) (- found-size size))
            (poke! found-addr size))
           (else
            (poke! found-addr found-size)))
         (tag found-addr)))))
 
 (define (vector-free vctr)
   (define addr (untag vctr))
   (define siz (peek addr))
   (let
       ((right-addr (find-address (+ addr siz))))
     (if (not (null? right-addr))
       (let
           ((right-size (peek right-addr)))
         (when (= right-addr (+ addr siz))
           (delete-free right-addr)
           (scheme:set! siz (+ siz right-size))))))
   (let 
       ((left-addr (find-address addr)))
     (if (not (null? left-addr))
       (let
           ((left-size (peek left-addr)))
         (when (= (+ left-addr left-size) addr)
           (delete-free left-addr)
           (scheme:set! addr left-addr)
           (scheme:set! siz (+ siz left-size))))))
   (insert-free addr siz))
 
 (define (address+index addr index)
   (let
       ((size (- (peek addr) 1)))
     (if (or (< size 0)
             (>= index size))
       (error "index out of bounds" index)
       (+ addr index 1))))

 (define (vector-ref vctr index)
   (define addr (untag vctr))
   (peek (address+index addr index)))
 
 (define (vector-set! vctr index value)
   (define addr (untag vctr))
   (poke! (address+index addr index) value))
 
 (define (vector-length vctr)
   (define addr (untag vctr))
   (- (peek addr) 1))
 
 (initialize))