#lang r6rs
(library
 (interpretation)
 (export new-interpret set-antecedents! get-lit-antecedents mark-literals! marked?
         from-interpret interpretation? find-unassigned-atom latest-assigned-lit
         true? true! false? false! unknown? unknown!
         all-assigned? clause-value formula-value
         interpret->string)
 (import (srfi :9)
         (rnrs mutable-pairs (6))
         (rnrs base)
         (rnrs io simple)
         (rnrs control)
         (rnrs lists)
         (a-d sat cnf))
 
 ; Interpretation ADT
 ; A "set" of VARIABLES represented by an "atom-indexed" vector.
 ; For each index a truth value is stored.
 (define-record-type interpretation
   (make-interpret s m l)
   interpretation?
   (s storage) ; vector
   (m marks) ; vector
   (l latest-assigned-lit latest-assigned-lit!)) ; number

 ; Default constructor
 (define (new-interpret formula)
   (make-interpret (make-vector (number-of-vars formula) unknown)
                   (make-vector (number-of-vars formula) (cons #f #f))
                   ()))

 ; Copy constructor
 (define (from-interpret interpret)
   (let ((new-storage (make-vector (vector-length (storage interpret)))))
     (let vector-copy
       ((i 0))
       (when (< i (vector-length new-storage))
         (vector-set! new-storage i (vector-ref (storage interpret) i))
         (vector-copy (+ i 1))))
     (make-interpret new-storage (latest-assigned-lit interpret))))

 (define (interpret-size interpret)
   (vector-length (storage interpret)))

 ;; Memorize antecedents of literal
 (define (set-antecedents! lit lst interpret)
   (set-cdr! (vector-ref (marks interpret) (- (variable lit) 1)) lst))

 ;; Get antecedents of propagated literal
 (define (get-lit-antecedents lit interpret)
   (cdr (vector-ref (marks interpret) (- (variable lit) 1))))
 
 ;; Mark a single literal
 (define (mark-lit! l interpret)
   (set-car! (vector-ref (marks interpret) (- (variable l) 1)) #t))

 (define (marked? lit interpret)
   (car (vector-ref (marks interpret) (- (variable lit) 1))))

 ;; Mark all the literals from the clause
 (define (mark-literals! interpret clause)
   (for-all (lambda (lit)
                     (mark-lit! lit interpret))
            (literals clause)))

 (define (find-unassigned-atom interpret) ; sequential search
   (let loop
     ((i 0))
     (if (< i (interpret-size interpret))
         (if (eq? (vector-ref (storage interpret) i)
                  unknown)
             (make-literal (+ i 1) #f) ; return a non-negated literal
             (loop (+ i 1)))
         #f)))

 (define (atom-value interpret l)
   (vector-ref (storage interpret) (- (variable l) 1)))

 (define (true? interpret lit)
   (eq? (atom-value interpret lit) (if (negation? lit)
                                       false
                                       true)))

 (define (false? interpret lit)
   (eq? (atom-value interpret lit) (if (negation? lit)
                                       true
                                       false)))
 
 (define (unknown? interpret lit)
   (eq? (atom-value interpret lit) unknown))

 (define (atom-value! interpret l val)
   (vector-set! (storage interpret) (- (variable l) 1) val)
   (latest-assigned-lit! interpret l)
   interpret)

 (define (true! interpret l)
   (atom-value! interpret l (if (negation? l)
                                false
                                true)))

 (define (false! interpret l)
   (atom-value! interpret l (if (negation? l)
                                true
                                false)))

 (define (unknown! interpret l)
   (atom-value! interpret l unknown))

 (define (all-assigned? interpret)
   (let loop
     ((i 1))
     (if (> i (interpret-size interpret))
         #t
         (and (not (unknown? interpret (make-literal i 'ignored)))
              (loop (+ i 1))))))

 (define (clause-value c interpret)
   (cond ((exists (lambda (lit)
                    (eq? (atom-value interpret lit)
                         (if (negation? lit)
                             false
                             true)))
                  (literals c))
          true)
         ((for-all (lambda (lit)
                     (eq? (atom-value interpret lit)
                          (if (negation? lit)
                              true
                              false)))
            (literals c))
          false)
         (else unknown)))
 
 (define (formula-value f interpret)
   (cond ((for-all (lambda (clause)
                     (eq? (clause-value clause interpret)
                          true))
            (clauses f))
          true)
         ((exists (lambda (clause)
                    (eq? (clause-value clause interpret)
                         false))
                  (clauses f))
          false)
         (else unknown)))
 
 (define (interpret->string interpret)
   (apply string-append (append (list "{")
                                (let loop
                                  ((i 0)
                                   (res ()))
                                  (if (< i (interpret-size interpret))
                                      (let ((val (vector-ref (storage interpret) i)))
                                        (if (eq? val unknown)
                                            (loop (+ i 1) res)
                                            (loop (+ i 1)
                                                  (cons (literal->string (make-literal (+ i 1) (not val)))
                                                        (cons " "
                                                              res)))))
                                      (reverse res))) ; to restore the order of the the literals
                                (list " }"))))
 )