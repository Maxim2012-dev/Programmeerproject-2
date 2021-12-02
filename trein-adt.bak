#lang racket

(require "simulator/interface.rkt")
(provide maak-trein)

; We stellen een trein voor door een abstractie ervan te maken.
(define (maak-trein id prev-seg cur-seg)
  (let ((trein-id id)
        (richting prev-seg) ; bepaald op basis van prev-seg
        (startsegment cur-seg)
        (snelheid 0))


    ; snelheid van huidige trein
    (define (huidige-snelheid)
      snelheid)

    ; verander snelheid van huidige trein
    (define (verander-snelheid! nieuwe-snelheid)
      (set! snelheid nieuwe-snelheid)
      (set-loco-speed! trein-id nieuwe-snelheid))

    ; dispatch-procedure
    (define (dispatch-trein msg)
      (cond ((eq? msg 'trein-id) trein-id)
            ((eq? msg 'richting) prev-seg)
            ((eq? msg 'startsegment) startsegment)
            ((eq? msg 'huidige-snelheid) huidige-snelheid)
            ((eq? msg 'verander-snelheid!) verander-snelheid!)
            (else (display "foute boodschap - trein-adt"))))
    dispatch-trein))