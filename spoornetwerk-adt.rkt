#lang racket

(require "simulator/interface.rkt")

(define (maak-spoornetwerk)
  (let ((wissel-ids (get-switch-ids))
        (wissels '()) ; effectieve wissels om stand te manipuleren
        (detectieblok-ids (get-detection-block-ids)))


    ; een wissel van het type 'wissel' toevoegen aan de lijst
    ; (manueel wissels aanmaken)
    (define (voeg-wissel-toe! wissel)
      (set! wissels (cons wissel wissels)))

    ; positie van switch met id wijzigen
    (define (wijzig-stand-switch! id stand)
      (define (iter wissels)
          (when (not (null? (cdr wissels)))
              (cond ((eq? id ((cadr wissels) 'wissel-id))
                     (((cadr wissels) 'verander-stand!) stand)) 
                    (else (iter (cdr wissels))))))
      (iter wissels))

    (define (dispatch-spoornetwerk msg)
      (cond ((eq? msg 'wissel-ids) wissel-ids)
            ((eq? msg 'wissels) wissels)
            ((eq? msg 'detectieblok-ids) detectieblok-ids)
            ((eq? msg 'voeg-wissel-toe!) voeg-wissel-toe!)
            ((eq? msg 'wijzig-stand-switch!) wijzig-stand-switch!)
            (else (display "foute boodschap - spoornetwerk-adt"))))
    dispatch-spoornetwerk))
                                                         