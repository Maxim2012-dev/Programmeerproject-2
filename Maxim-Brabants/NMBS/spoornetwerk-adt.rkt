#lang racket

(require "treinreeks-adt.rkt")

(provide maak-spoornetwerk)

(define (maak-spoornetwerk)
  (let ((wissel-ids '())
        (wissels '()) ; effectieve wissels om stand te manipuleren
        (aanwezige-treinen (maak-treinreeks))
        (detectieblok-ids '()))


    ; een wissel van het type 'wissel' toevoegen aan de lijst
    ; (manueel wissels aanmaken)
    (define (voeg-wissel-toe! wissel)
      (set! wissels (cons wissel wissels)))

    ; reeks van treinen die op het spoor staan toevoegen
    (define (voeg-nieuwe-trein-toe! trein)
      ((aanwezige-treinen 'voeg-trein-toe!) trein))

    (define (geef-trein-ids)
      (map (lambda (el) (el 'trein-id))
           (aanwezige-treinen 'reeks)))

    (define (geef-trein-snelheden)
      (map (lambda (el) (el 'snelheid))
           (aanwezige-treinen 'reeks)))

    (define (set-wissel-ids! ids)
      (set! wissel-ids ids))

    (define (set-detectieblok-ids! ids)
      (set! detectieblok-ids ids))

    ; dispatch-procedure
    (define (dispatch-spoornetwerk msg . args)
      (cond ((eq? msg 'wissel-ids) wissel-ids)
            ((eq? msg 'wissels) wissels)
            ((eq? msg 'geef-trein-ids) (geef-trein-ids))
            ((eq? msg 'geef-trein-snelheden) (geef-trein-snelheden))
            ((eq? msg 'aanwezige-treinen) aanwezige-treinen)
            ((eq? msg 'detectieblok-ids) detectieblok-ids)
            ((eq? msg 'voeg-wissel-toe!) voeg-wissel-toe!)
            ((eq? msg 'voeg-nieuwe-trein-toe!) voeg-nieuwe-trein-toe!)
            ((eq? msg 'set-wissel-ids!) (set-wissel-ids! (car args)))
            ((eq? msg 'set-detectieblok-ids!) (set-detectieblok-ids! (car args)))
            (else (display "foute boodschap - spoornetwerk-adt - nmbs"))))
    dispatch-spoornetwerk))
                                                         