#lang racket

(provide maak-trein)

(define SNELHEIDSVERANDERING 20)

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
    (define (verander-snelheid! action)
      (cond ((eq? action '+)
             (set! snelheid (+ snelheid SNELHEIDSVERANDERING)))
            ((eq? action '-)
             (set! snelheid (+ snelheid SNELHEIDSVERANDERING)))
            (else (display "wrong-action-trein-adt"))))

    ; dispatch-procedure
    (define (dispatch-trein msg . args)
      (cond ((eq? msg 'trein-id) trein-id)
            ((eq? msg 'richting) prev-seg)
            ((eq? msg 'startsegment) startsegment)
            ((eq? msg 'huidige-snelheid) huidige-snelheid)
            ((eq? msg 'verander-snelheid!) (verander-snelheid! (car args)))
            (else (display "foute boodschap - trein-adt"))))
    dispatch-trein))