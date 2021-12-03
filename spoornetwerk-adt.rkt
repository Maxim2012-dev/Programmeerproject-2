#lang racket

(require "simulator/interface.rkt")
(require "treinreeks-adt.rkt")

(provide maak-spoornetwerk)

(define (maak-spoornetwerk)
  (let ((wissel-ids (get-switch-ids))
        (wissels '()) ; effectieve wissels om stand te manipuleren
        (aanwezige-treinen (maak-treinreeks))
        (detectieblok-ids (get-detection-block-ids)))


    ; een wissel van het type 'wissel' toevoegen aan de lijst
    ; (manueel wissels aanmaken)
    (define (voeg-wissel-toe! wissel)
      (set! wissels (cons wissel wissels)))

    ; reeks van treinen die op het spoor staan toevoegen
    (define (voeg-rijdende-trein-toe! trein)
      ((aanwezige-treinen 'voeg-trein-toe!) trein))

    ; positie van switch met id wijzigen
    (define (wijzig-stand-switch! id stand)
      (define (iter wissels)
          (when (not (null? (cdr wissels)))
              (cond ((eq? id ((cadr wissels) 'wissel-id))
                     (((cadr wissels) 'verander-stand!) stand)) 
                    (else (iter (cdr wissels))))))
      (iter wissels))

    ; dispatch-procedure
    (define (dispatch-spoornetwerk msg)
      (cond ((eq? msg 'wissel-ids) wissel-ids)
            ((eq? msg 'wissels) wissels)
            ((eq? msg 'detectieblok-ids) detectieblok-ids)
            ((eq? msg 'voeg-wissel-toe!) voeg-wissel-toe!)
            ((eq? msg 'wijzig-stand-switch!) wijzig-stand-switch!)
            (else (display "foute boodschap - spoornetwerk-adt"))))
    dispatch-spoornetwerk))
                                                         