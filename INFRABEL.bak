#lang racket

(require "simulator/interface.rkt")
(require "trein-adt.rkt")
(require "treinreeks-adt.rkt")
(require "wissel-adt.rkt")
(require "spoornetwerk-adt.rkt")

(provide maak-programma)


(define (maak-programma)
  (let ((spoor #f))


    (define (start-programma setup-functie)
      (setup-functie)
      (set! spoor (maak-spoornetwerk))
      (start))


    (define (zet-trein-op-spoor id richting segment)
      (let ((id-symbol (string->symbol id))
            (richting-symbol (string->symbol richting))
            (segment-symbol (string->symbol segment)))
        (when spoor
          ((spoor 'voeg-nieuwe-trein-toe!)
           (maak-trein id-symbol richting-symbol segment-symbol)))))


    (define (dispatch-programma msg)
      (cond ((eq? msg 'start-programma) start-programma)
            ((eq? msg 'zet-trein-op-spoor) zet-trein-op-spoor)
            (else (display "foute boodschap - INFRABEL"))))
    dispatch-programma))




