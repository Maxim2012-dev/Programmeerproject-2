#lang racket

(require "trein-adt.rkt")
(require "spoornetwerk-adt.rkt")
(require "../connection-API.rkt")
(require "../GUI.rkt")

(require "a-d/graph/unweighted/adjacency-matrix.rkt")
(require "a-d/graph-algorithms/undirected/bft-applications.rkt")

(provide maak-nmbs)


;; ==================== TCP SETUP (client) ====================
(define-values (in out) (tcp-connect "localhost" 9883))
(displayln "(NMBS:: Attempting to connect to server...)")
(if (and (tcp-port? in) (tcp-port? out))
    (displayln "(NMBS:: Successfully connected to server! (INFRABEL))")
    (displayln "(NMBS:: Connection failed)"))

(define SNELHEIDSVERANDERING 20)


(define (maak-nmbs) 
  (let ((GUI #f)
        (spoor #f))               ;; rail network needs to be received from INFRABEL

    (request-rail-network out)    ;; request the rail network from INFRABEL

    (define (read-from-input-port)
      (let ((input (read in)))
        (displayln input)
        (cond ((eq? (car input) 'rail-network)
           (set! spoor (cadr input)))
          (else (display "wrong-message")))
        (read-from-input-port)))
    (thread read-from-input-port)
    

    ;; vector met spoorcomponenten
    (define componenten (vector 'S2 'D1 'D2 'D3 'D4 'D5 'D6 'D7 'D8 'D9 'S1 'S3))

    ;; graafrepresentatie van spooropstelling
    ;; (we zien de detectieblokken en de wissels als nodes)
    (define spoornetwerk
      (let ((g (new #f 12)))
        (add-edge! g 0 1)
        (add-edge! g 0 6)
        (add-edge! g 0 7)
        (add-edge! g 1 2)
        (add-edge! g 2 3)
        (add-edge! g 3 4)
        (add-edge! g 4 10)
        (add-edge! g 10 5)
        (add-edge! g 5 6)
        (add-edge! g 10 11)
        (add-edge! g 11 8)
        (add-edge! g 8 9)
        (add-edge! g 11 7)
        g))

    ;; (functionaliteit voor de simulator)
    (define (zet-trein-op-spoor! id richting segment)
      (let ((id-symbol (string->symbol id))
            (richting-symbol (string->symbol richting))
            (segment-symbol (string->symbol segment)))
        (when (and spoor GUI)
          (let ((nieuwe-trein (maak-trein id-symbol richting-symbol segment-symbol)))
            ((spoor 'voeg-nieuwe-trein-toe!) nieuwe-trein)
            (GUI 'teken-trein-in-panel id)
            (send-train-message nieuwe-trein out)))))         ;; synchroniseren met infrabel via TCP


    ;; nieuwe client aan client manager laten toevoegen door infrabel
    (define (voeg-nieuwe-client-toe)
      (send-new-client (maak-nmbs) out))


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

    ;; berekent een traject voor trein met 'id' naar 'destination'
    (define (bereken-traject id destination)
      (let* ((block (((spoor 'aanwezige-treinen) 'detectieblok-trein) id))
             (from (geef-vector-index block))
             (to (geef-vector-index destination)))
        (shortest-path spoornetwerk from to)))

    ;; geeft de index van gegeven detectieblok in componentenvector
    (define (geef-vector-index detectieblok)
      (let iter
        ((idx 0))
        (when (not (>= idx (vector-length componenten)))
          (if (eq? detectieblok (vector-ref componenten idx))
              idx
              (iter (+ idx 1))))))

    (define (geef-aanwezige-treinen)
      (spoor 'aanwezige-treinen))

    (define (geef-trein-ids)
      (map (lambda (el) (el 'trein-id))
           ((spoor 'aanwezige-treinen) 'reeks)))

    (define (geef-wissel-ids)
      (spoor 'wissel-ids))

    (define (geef-detectieblok-ids)
      (spoor 'detectieblok-ids))

    (define (verander-wisselstand! id stand)
      ((spoor 'wijzig-stand-switch!) id stand)
      (send-change-switch id stand out))                ;; synchroniseren met infrabel via TCP

    (define (dispatch-nmbs msg . args)
      (cond ((eq? msg 'zet-trein-op-spoor!) (zet-trein-op-spoor! (car args) (cadr args) (caddr args)))
            ((eq? msg 'verhoog-snelheid-trein!) (verhoog-snelheid-trein! (car args)))
            ((eq? msg 'verlaag-snelheid-trein!) (verlaag-snelheid-trein! (car args)))
            ((eq? msg 'geef-snelheid-trein) (geef-snelheid-trein (car args)))
            ((eq? msg 'bereken-traject) (bereken-traject (car args) (cadr args)))
            ((eq? msg 'geef-wissel-ids) (geef-wissel-ids))
            ((eq? msg 'geef-aanwezige-treinen) (geef-aanwezige-treinen))
            ((eq? msg 'geef-trein-ids) (geef-trein-ids))
            ((eq? msg 'geef-detectieblok-ids) (geef-detectieblok-ids))
            ((eq? msg 'verander-wisselstand!) (verander-wisselstand! (car args) (cadr args)))
            ((eq? msg 'voeg-nieuwe-client-toe) (voeg-nieuwe-client-toe))
            (else (display "foute boodschap - NMBS"))))

    (set! GUI (maak-gui dispatch-nmbs))
    (GUI 'start)
    (GUI 'teken-wissel-panel)
    (GUI 'teken-detectieblok-panel)

    (thread (GUI 'refresh-detection-blocks))
    
    dispatch-nmbs))