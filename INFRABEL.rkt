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
      (let ((nieuwe-trein (maak-trein id richting segment)))
        (when spoor
            ((spoor 'voeg-nieuwe-trein-toe!) nieuwe-trein))))


    (define (dispatch-programma msg)
      (cond ((eq? msg 'start-programma) start-programma)
            (else (display "foute boodschap - INFRABEL"))))
    dispatch-programma))




