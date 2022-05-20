#lang racket

(require "../simulator/interface.rkt")
(require "../connection-API.rkt")
(require "trein-adt.rkt")
(require "treinreeks-adt.rkt")
(require "wissel-adt.rkt")
(require "spoornetwerk-adt.rkt")

(define SNELHEIDSVERANDERING 20)


;; ==============================================================
;; ========================  INFRABEL  ==========================
;; ==============================================================

 
(setup-loop-and-switches)
(start)


;; ==================== TCP SETUP (server) ======================
(define listener (tcp-listen 9883 4 #t))
(define-values (in out) (tcp-accept listener))
(displayln "Connection in progress...")
(if (and in out)
    (displayln "INFRABEL successfully connected to client!")
    (displayln "Connection failed."))


(define spoor (maak-spoornetwerk))

(define (read-from-input-port)
  (let ((input (read in)))
    (displayln input)
    (cond ((eq? (car input) 'rail-network)
           (send-rail-network spoor out))
          (else (display "wrong-message")))
    (read-from-input-port)))
(thread read-from-input-port)                        ;; keeps reading the input port
    

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

(define (geef-wissel-ids)
  (spoor 'wissel-ids))

(define (geef-detectieblok-ids)
  (spoor 'detectieblok-ids))

(define (verander-wisselstand! id stand)
  ((spoor 'wijzig-stand-switch!) id stand))




