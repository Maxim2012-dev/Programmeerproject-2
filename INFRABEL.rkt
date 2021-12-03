#lang racket

(require "simulator/interface.rkt")
(require "trein-adt.rkt")
(require "treinreeks-adt.rkt")
(require "wissel-adt.rkt")
(require "spoornetwerk-adt.rkt")

(provide maak-programma)


(define (maak-programma)
  (let ((spoor (maak-spoornetwerk)))


    (define (start-programma-straight)
      (setup-straight)
      (start))

    (define (start-programma-loop)
      (setup-loop)
      (start))

    (define (start-programma-loop-and-switches)
      (setup-loop-and-switches)
      (start))

    (define (start-programma-straight-with-switch)
      (setup-straight-with-switch)
      (start))


    (define (dispatch-programma msg)
      (cond ((eq? msg 'start-programma-straight) start-programma-straight)
            ((eq? msg 'start-programma-loop) start-programma-loop)
            ((eq? msg 'start-programma-loop-and-switches) start-programma-loop-and-switches)
            ((eq? msg 'start-programma-straight-with-switch) start-programma-straight-with-switch)
            (else (display "foute boodschap - INFRABEL"))))
    dispatch-programma))




