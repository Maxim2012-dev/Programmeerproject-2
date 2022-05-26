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

;; gedeeld spoornetwerk over alle clients
(define spoor (maak-spoornetwerk))

(define (maak-nmbs manager)
  (let ((GUI #f)
        (client-manager manager))

    ;; request railway components from INFRABEL
    (request-switch-ids out)
    (request-detection-block-ids out)
    
    (define (read-from-input-port)
      (let ((input (read in)))
        (cond ((eq? (car input) 'switch-ids)                                                 ;; wissel-ids ontvangen
               (spoor 'set-wissel-ids! (cdr input)))
              ((eq? (car input) 'detection-block-ids)                                        ;; detectieblok-ids ontvangen
               (spoor 'set-detectieblok-ids! (cdr input)))
              ((eq? (car input) 'draw-train)                                                 ;; nieuwe trein tekenen in panel
               (when (symbol? (cadr input))
                   (GUI 'teken-trein-in-panel (symbol->string (cadr input)))))
              ((eq? (car input) 'draw-train-speed)                                           ;; snelheid van trein tekenen
               (GUI 'teken-trein-snelheid (cadr input) (caddr input)))
              ((eq? (car input) 'draw-loco-block)                                            ;; status van detectieblok tekenen
               (when GUI
                 (GUI 'teken-detectieblok-status (cadr input) (caddr input))))
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

    (define (zet-trein-op-spoor! id richting segment)
      (let ((id-symbol (string->symbol id))
            (richting-symbol (string->symbol richting))
            (segment-symbol (string->symbol segment)))
        (when (and spoor GUI)
            ((spoor 'voeg-nieuwe-trein-toe!) (maak-trein id-symbol richting-symbol segment-symbol))
            (send-train-message id-symbol richting-symbol segment-symbol out))))           ;; gegevens voor nieuwe trein doorsturen naar infrabel


    ;; nieuwe client aan client manager toevoegen
    (define (voeg-nieuwe-client-toe)
      (manager 'add-new-client (maak-nmbs manager) (spoor 'geef-trein-ids)))


    (define (verhoog-snelheid-trein! trein-id)
      (let* ((id-symbol (string->symbol trein-id)))
        (send-change-train-speed id-symbol '+ out)
        ((spoor 'aanwezige-treinen) 'wijzig-snelheid-trein! trein-id '+)))

    (define (verlaag-snelheid-trein! trein-id)
      (let* ((id-symbol (string->symbol trein-id)))
        (send-change-train-speed id-symbol '- out)
        ((spoor 'aanwezige-treinen) 'wijzig-snelheid-trein! trein-id '-)))


    (define (geef-snelheid-trein trein-id)
      (let ((id-symbol (string->symbol trein-id)))
        (request-train-speed id-symbol out)))

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

    (define (geef-wissel-ids)
      (spoor 'wissel-ids))

    (define (geef-detectieblok-ids)
      (spoor 'detectieblok-ids))

    (define (detectieblok-trein trein-id)
      (request-loco-detection-block trein-id out))

    (define (verander-wisselstand! id stand)
      (send-change-switch id stand out))                ;; ask INFRABEL to change switch status

    (define (dispatch-nmbs msg . args)
      (cond ((eq? msg 'GUI) GUI)
            ((eq? msg 'spoor) spoor)
            ((eq? msg 'zet-trein-op-spoor!) (zet-trein-op-spoor! (car args) (cadr args) (caddr args)))
            ((eq? msg 'verhoog-snelheid-trein!) (verhoog-snelheid-trein! (car args)))
            ((eq? msg 'verlaag-snelheid-trein!) (verlaag-snelheid-trein! (car args)))
            ((eq? msg 'geef-snelheid-trein) (geef-snelheid-trein (car args)))
            ((eq? msg 'bereken-traject) (bereken-traject (car args) (cadr args)))
            ((eq? msg 'geef-wissel-ids) (geef-wissel-ids))
            ((eq? msg 'geef-aanwezige-treinen) (geef-aanwezige-treinen))
            ((eq? msg 'geef-detectieblok-ids) (geef-detectieblok-ids))
            ((eq? msg 'verander-wisselstand!) (verander-wisselstand! (car args) (cadr args)))
            ((eq? msg 'detectieblok-trein) (detectieblok-trein (car args)))
            ((eq? msg 'voeg-nieuwe-client-toe) (voeg-nieuwe-client-toe))
            (else (display "foute boodschap - NMBS"))))

    
    (set! GUI (maak-gui dispatch-nmbs))
    (GUI 'start)
    (GUI 'teken-wissel-panel)
    (GUI 'teken-detectieblok-panel)
    
    dispatch-nmbs))