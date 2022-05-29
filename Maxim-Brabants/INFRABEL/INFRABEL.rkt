#lang racket

(require "../simulator/interface.rkt")
(require "../simulator/railway.rkt")
(require "../simulator/simulator.rkt")
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
(if (and (tcp-port? in) (tcp-port? out))
    (displayln "INFRABEL successfully connected to client!")
    (displayln "Connection failed."))

;; Spoor + Client Manager
(define spoor (maak-spoornetwerk))

;; proces dat van de input port blijft lezen
(define (read-from-input-port)
  (let ((input (read in)))
    (cond ((eq? (car input) 'switch-status)                                                     ;; change the status of a switch
           (verander-wisselstand! (cadr input) (caddr input)))
          ((eq? (car input) 'switch-ids)                                                        ;; return ids of switches from railway
           (send-switch-ids (spoor 'wissel-ids) out))
          ((eq? (car input) 'detection-block-ids)                                               ;; return ids of detection blocks from railway
           (send-detection-block-ids (spoor 'detectieblok-ids) out))
          ((eq? (car input) 'new-train)                                                         ;; put new train on the tracks (simulator)
           (zet-trein-op-spoor! (cadr input) (caddr input) (cadddr input)))
          ((eq? (car input) 'train-speed)                                                       ;; returns speed of a train
           (send-draw-train-speed (geef-snelheid-trein (cadr input)) (cadr input) out))
          ((eq? (car input) 'loco-block)                                                        ;; returns detection block of a train
           (let ((loco-block (((spoor 'aanwezige-treinen) 'detectieblok-trein) (cadr input))))
             (when loco-block
               (send-draw-loco-block (cadr input) loco-block out))))
          ((eq? (car input) 'change-speed)                                                      ;; changes speed of a train
           (dispatch-change-speed (cadr input) (caddr input)))
          ((eq? (car input) 'formation)                                                         ;; nieuwe opstelling krijgen van NMBS
           (initialize-formation (cadr input)))
          (else (display "wrong-message")))
    (read-from-input-port)))
(thread read-from-input-port)                        ;; keeps reading the input port
    

;; (functionaliteit voor de simulator)
(define (zet-trein-op-spoor! id richting segment)
  (when spoor
    ((spoor 'voeg-nieuwe-trein-toe!)
     (maak-trein id richting segment))
    (send-draw-train id out)))

;; simulator aanroepen om de opstelling te initialiseren
(define (initialize-formation formation)
  (displayln formation)
  (stop)
  (load-custom-setup (car formation) (cadr formation) (caddr formation))
  (start))

(define (dispatch-change-speed id action)
  (cond ((eq? action '+) (verhoog-snelheid-trein! id))
        ((eq? action '-) (verlaag-snelheid-trein! id))
        (else (displayln "wrong-action-dispatch"))))

(define (verhoog-snelheid-trein! trein-id)
  (let* ((aanwezige-treinen (spoor 'aanwezige-treinen))
         (treinsnelheid ((aanwezige-treinen 'snelheid-trein) trein-id)))
    ((aanwezige-treinen 'wijzig-snelheid-trein!) trein-id (+ treinsnelheid SNELHEIDSVERANDERING))
    (send-draw-train-speed trein-id (geef-snelheid-trein trein-id) out)))

(define (verlaag-snelheid-trein! trein-id)
  (let* ((aanwezige-treinen (spoor 'aanwezige-treinen))
         (treinsnelheid ((aanwezige-treinen 'snelheid-trein) trein-id)))
    ((aanwezige-treinen 'wijzig-snelheid-trein!) trein-id (- treinsnelheid SNELHEIDSVERANDERING))
    (send-draw-train-speed trein-id (geef-snelheid-trein trein-id) out)))


(define (geef-snelheid-trein trein-id)
  (let ((aanwezige-treinen (spoor 'aanwezige-treinen)))
    ((aanwezige-treinen 'snelheid-trein) trein-id)))

(define (geef-wissel-ids)
  (spoor 'wissel-ids))

(define (geef-detectieblok-ids)
  (spoor 'detectieblok-ids))

(define (verander-wisselstand! id stand)
  (let ((number-stand (string->number stand)))
    ((spoor 'wijzig-stand-switch!) id number-stand)))




