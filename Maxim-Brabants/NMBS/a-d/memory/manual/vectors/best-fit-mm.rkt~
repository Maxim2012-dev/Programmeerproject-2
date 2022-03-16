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
         peek poke! make memory-size reset-free lowest-size insert-free address)
 (import  
  (rnrs control)
  (rename (except (rnrs base) vector? vector-length)
          (set! scheme:set!) (vector-set! scheme:vector-set!) (vector-ref scheme:vector-ref) (make-vector scheme:make-vector)))
 
 (define null ())
 (define lowest-size 5)
 
 (define memory-size 200)
 (define memory (scheme:make-vector memory-size null))
 
 (define (peek addr)
   (scheme:vector-ref memory addr))
 
 (define (poke! addr value)
   (scheme:vector-set! memory addr value))
 
 (define address-tree null)
 (define size-tree null)
 
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
       ((eq? curr null)
        null)
       ((= curr addr)
        curr)
       ((< addr curr)
        (lookup (address-left curr)))
       (else
        (let ((try (lookup (address-right curr))))
          (if (eq? try null)
            curr
            try))))))
 
 (define (find-size req-size) 
   (let lookup 
     ((curr size-tree))
     (cond 
       ((eq? curr null)
        null)
       ((= (size curr) req-size)
        curr)
       ((< (size curr) req-size)
        (lookup (size-right curr)))
       (else 
        (let ((try (lookup (size-left curr))))
          (if (eq? try null)
            curr
            try))))))
 
 (define (insert-free addr siz)
   (define (insert-address curr)
     (if (< curr addr)
       (let 
           ((right (address-right curr)))
         (if (eq? right null)
           (address-right! curr addr)
           (insert-address right)))
       (let 
           ((left (address-left curr)))
         (if (eq? left null)
           (address-left! curr addr)
           (insert-address left)))))
   (define (insert-size curr)
     (if (>= (size curr) siz)
       (let 
           ((left (size-left curr)))
         (if (eq? left null)
           (size-left! curr addr)
           (insert-size left)))
       (let 
           ((right (size-right curr)))
         (if (eq? right null)
           (size-right! curr addr)
           (insert-size right)))))
   (size! addr siz)
   (address-left! addr null)
   (address-right! addr null)
   (size-left! addr null)
   (size-right! addr null)
   (if (eq? address-tree null)
     (scheme:set! address-tree addr)
     (insert-address address-tree))
   (if (eq? size-tree null)
     (scheme:set! size-tree addr)
     (insert-size size-tree)))
 
 (define (delete-free addr)
   (define siz (size addr))
   (define (delete-next-address curr op)
     (cond
       ((eq? (address-right curr) null)
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
       ((eq? (address-left curr) null)
        (op (address-right curr)))
       ((eq? (address-right curr) null)
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
       ((eq? (size-left curr) null)
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
       ((eq? (size-left curr) null)
        (op (size-right curr)))
       ((eq? (size-right curr) null)
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
 
 (define (reset-free)
   (scheme:set! address-tree null)
   (scheme:set! size-tree   null))
 
 (define (initialize)
   (insert-free 0 memory-size))
 
 (define vector-tag 'vector) 
 
 (define (make addr)
   (cons vector-tag addr))
 
 (define (address vctr)
   (cdr vctr))
 
 (define (vector? any)
   (and (pair? any)
        (eq? (car any) vector-tag)))
 
 (define (make-vector size)
   (scheme:set! size (+ size 1))
   (if (< size lowest-size)
     (scheme:set! size lowest-size))
   (let
       ((hold-addr (find-size size))) 
     (if (eq? hold-addr null)
       null
       (let
           ((hold-size (peek hold-addr)))
         (delete-free hold-addr)
         (cond
           ((>= (- hold-size size) lowest-size)
            (insert-free (+ hold-addr size) (- hold-size size))
            (poke! hold-addr size))
           (else
            (poke! hold-addr hold-size)))
         (make hold-addr)))))
 
 (define (vector-free vctr)
   (define addr (address vctr))
   (define siz (peek addr))
   (let
       ((hold-addr (find-address (+ addr siz))))
     (if (not (eq? hold-addr null))
       (let
           ((hold-size (peek hold-addr)))
         (when (= hold-addr (+ addr siz))
           (delete-free hold-addr)
           (scheme:set! siz (+ siz hold-size))))))
   (let 
       ((hold-addr (find-address addr)))
     (if (not (eq? hold-addr null))
       (let
           ((hold-size (peek hold-addr)))
         (when (= (+ hold-addr hold-size) addr)
           (delete-free hold-addr)
           (scheme:set! addr hold-addr)
           (scheme:set! siz (+ siz hold-size))))))
   (insert-free addr siz))
 
 (define (address+index addr index)
   (if (number? index)
     (if (negative? index)
       (error "index must be non-negative" index)
       (let
           ((size (- (peek addr) 1)))
         (if (>= index size)
           (error "index out of bounds" index)
           (+ addr index 1))))
     (error "index must numerical" index)))
 
 (define (vector-ref vctr index)
   (define addr (address vctr))
   (peek (address+index addr index)))
 
 (define (vector-set! vctr index value)
   (define addr (address vctr))
   (poke! (address+index addr index) value))
 
 (define (vector-length vctr)
   (define addr (address vctr))
   (- (peek addr) 1))
 
 (initialize))