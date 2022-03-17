#lang racket


(require rnrs/mutable-pairs-6)
(require "../simulator/interface.rkt")
(provide maak-treinreeks)

; abstractie voor de verzameling van aanwezige treinen
(define (maak-treinreeks)
  (let ((reeks '()))


    ; trein aan reeks en spoor toevoegen
    (define (voeg-trein-toe! trein)
      (let ((id (trein 'trein-id))
            (prev-seg (trein 'richting))
            (cur-seg (trein 'startsegment)))
        (set! reeks (cons trein reeks))
        (add-loco id prev-seg cur-seg)))


    ; trein van reeks en spoor verwijderen
    (define (verwijder-trein! trein)
      (let ((trein-id (trein 'trein-id)))
        (define (iter reeks)
          (when (not (null? (cdr reeks)))
              (cond ((eq? trein (cadr reeks))
                     (set-cdr! reeks (cddr reeks)))
                    (else (iter (cdr reeks))))))
        (iter reeks)
        (remove-loco trein-id)))


    ; snelheid van trein met id opvragen
    (define (snelheid-trein id)
      (get-loco-speed id))

    ; snelheid van trein met id wijzigen
    (define (wijzig-snelheid-trein! id snelheid)
      (define (iter reeks)
          (when (not (null? (cdr reeks)))
              (cond ((eq? id ((cadr reeks) 'trein-id))
                     (((cadr reeks) 'verander-snelheid!) snelheid)) 
                    (else (iter (cdr reeks))))))
      (iter reeks)
      (set-loco-speed! id snelheid))

    ; detectieblok van trein met id
    (define (detectieblok-trein trein-id)
      (get-loco-detection-block trein-id))

    
    ; dispatch-procedure
    (define (dispatch-treinreeks msg)
      (cond ((eq? msg 'voeg-trein-toe!) voeg-trein-toe!)
            ((eq? msg 'verwijder-trein!) verwijder-trein!)
            ((eq? msg 'snelheid-trein) snelheid-trein)
            ((eq? msg 'wijzig-snelheid-trein!) wijzig-snelheid-trein!)
            ((eq? msg 'detectieblok-trein) detectieblok-trein)
            (else (display "foute boodschap - treinreeks-adt"))))
    dispatch-treinreeks))
      
        
        