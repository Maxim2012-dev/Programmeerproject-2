#lang racket

(require "simulator/interface.rkt")
(require "trein-adt.rkt")
(require "treinreeks-adt.rkt")
(require "wissel-adt.rkt")
(require "spoornetwerk-adt.rkt")

(define snelheidstrein (maak-trein 'T-1 'D6 'D7))
(define treinreeks (maak-treinreeks))


(define (set-speed-at train detection-block speed)
  (if (eq? (get-loco-detection-block train) detection-block)
      (set-loco-speed! train speed)
      (begin (sleep 1)
             (set-speed-at train detection-block speed))))

(define (test1)
  ((treinreeks 'voeg-trein-toe!) snelheidstrein)
  ((snelheidstrein 'verander-snelheid!) 300)

  (define (loop)
    (set-speed-at (snelheidstrein 'trein-id) 'D1 (- ((snelheidstrein 'huidige-snelheid))))
    ;(set-speed-at (snelheidstrein 'trein-id) 'D3 (- ((snelheidstrein 'huidige-snelheid))))
    (loop))
  (loop))


(setup-loop)
(start)

(thread test1)