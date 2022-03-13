#lang racket

(require "trein-adt.rkt")
(require "treinreeks-adt.rkt")
(require "wissel-adt.rkt")
(require "spoornetwerk-adt.rkt")

(provide maak-nmbs)

(define SNELHEIDSVERANDERING 20)

;; Dit draait op onze computer zelf
(define (maak-nmbs)
  (let ((spoor (maak-spoornetwerk)))


    ;; (functionaliteit voor de simulator)
    (define (zet-trein-op-spoor! id richting segment)
      (let ((id-symbol (string->symbol id))
            (richting-symbol (string->symbol richting))
            (segment-symbol (string->symbol segment)))
        (when spoor
          ((spoor 'voeg-nieuwe-trein-toe!)
           (maak-trein id-symbol richting-symbol segment-symbol)))))


    (define (verhoog-snelheid-trein! trein-id)
      (let* ((id-symbol (string->symbol trein-id))
             (aanwezige-treinen (spoor 'aanwezige-treinen))
             (treinsnelheid ((aanwezige-treinen 'snelheid-trein) id-symbol)))
        ((aanwezige-treinen 'wijzig-snelheid-trein!) id-symbol (+ treinsnelheid SNELHEIDSVERANDERING))))

    (define (verlaag-snelheid-trein! trein-id)
      (let* ((id-symbol (string->symbol trein-id))
             (aanwezige-treinen (spoor 'aanwezige-treinen))
             (treinsnelheid ((aanwezige-treinen 'snelheid-trein) id-symbol)))
        ((aanwezige-treinen 'wijzig-snelheid-trein!) id-symbol (- treinsnelheid SNELHEIDSVERANDERING))))


    (define (geef-snelheid-trein trein-id)
      (let ((id-symbol (string->symbol trein-id))
            (aanwezige-treinen (spoor 'aanwezige-treinen)))
        ((aanwezige-treinen 'snelheid-trein) id-symbol)))

    (define (geef-aanwezige-treinen)
      (spoor 'aanwezige-treinen))

    (define (geef-wissel-ids)
      (spoor 'wissel-ids))

    (define (geef-detectieblok-ids)
      (spoor 'detectieblok-ids))

    (define (verander-wisselstand! id stand)
      ((spoor 'wijzig-stand-switch!) id stand))

    (define (dispatch-nmbs msg)
      (cond ((eq? msg 'zet-trein-op-spoor) zet-trein-op-spoor!)
            ((eq? msg 'verhoog-snelheid-trein!) verhoog-snelheid-trein!)
            ((eq? msg 'verlaag-snelheid-trein!) verlaag-snelheid-trein!)
            ((eq? msg 'geef-snelheid-trein) geef-snelheid-trein)
            ((eq? msg 'geef-wissel-ids) geef-wissel-ids)
            ((eq? msg 'geef-aanwezige-treinen) geef-aanwezige-treinen)
            ((eq? msg 'geef-detectieblok-ids) geef-detectieblok-ids)
            ((eq? msg 'verander-wisselstand!) verander-wisselstand!)
            (else (display "foute boodschap - INFRABEL"))))
    dispatch-nmbs))