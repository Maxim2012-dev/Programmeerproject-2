#lang racket

(require "../simulator/interface.rkt")

(define (maak-wissel id init-pos)
  (let ((wissel-id id)
        (stand init-pos))   ; stand 1 of 2


    ; stand van huidige switch opvragen
    (define (huidige-stand)
      (get-switch-position wissel-id))

    ; stand van huidige switch veranderen
    (define (verander-stand! nieuwe-stand)
      (set! stand nieuwe-stand)
      (set-switch-position! wissel-id nieuwe-stand))

    ; dispatch-procedure
    (define (dispatch-wissel msg)
      (cond ((eq? msg 'wissel-id) wissel-id)
            ((eq? msg 'huidige-stand) (huidige-stand))
            ((eq? msg 'verander-stand!) verander-stand!)
            (else (display "foute boodschap - wissel-adt"))))
    dispatch-wissel))