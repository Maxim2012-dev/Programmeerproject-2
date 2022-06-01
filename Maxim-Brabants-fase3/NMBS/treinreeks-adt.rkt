#lang racket


(require rnrs/mutable-pairs-6)
(provide maak-treinreeks)

; abstractie voor de verzameling van aanwezige treinen
(define (maak-treinreeks)
  (let ((reeks '()))


    ; trein aan reeks en spoor toevoegen
    (define (voeg-trein-toe! trein)
      (let ((id (trein 'trein-id))
            (prev-seg (trein 'richting))
            (cur-seg (trein 'startsegment)))
        (set! reeks (cons trein reeks))))


    ; trein van reeks en spoor verwijderen
    (define (verwijder-trein! trein)
      (let ((trein-id (trein 'trein-id)))
        (define (iter reeks)
          (when (not (null? (cdr reeks)))
              (cond ((eq? trein (cadr reeks))
                     (set-cdr! reeks (cddr reeks)))
                    (else (iter (cdr reeks))))))
        (iter reeks)))


    ; snelheid van trein met id wijzigen
    (define (wijzig-snelheid-trein! id action)
      (define (iter reeks)
          (when (not (null? reeks))
              (cond ((eq? id ((car reeks) 'trein-id))
                     ((car reeks) 'verander-snelheid! action))
                    (else (iter (cdr reeks))))))
      (iter reeks))

    ; snelheid van trein met id opvragen
    (define (geef-treinsnelheid id)
      (define (iter reeks)
          (when (not (null? reeks))
              (cond ((eq? id ((car reeks) 'trein-id))
                     ((car reeks) 'snelheid))
                    (else (iter (cdr reeks))))))
      (iter reeks))

    
    ; dispatch-procedure
    (define (dispatch-treinreeks msg . args)
      (cond ((eq? msg 'reeks) reeks)
            ((eq? msg 'voeg-trein-toe!) voeg-trein-toe!)
            ((eq? msg 'verwijder-trein!) verwijder-trein!)
            ((eq? msg 'wijzig-snelheid-trein!) (wijzig-snelheid-trein! (car args) (cadr args)))
            ((eq? msg 'geef-treinsnelheid) (geef-treinsnelheid (car args)))
            (else (display "foute boodschap - treinreeks-adt"))))
    dispatch-treinreeks))
      
        
        