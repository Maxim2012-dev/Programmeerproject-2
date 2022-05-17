#lang racket

(require "trein-adt.rkt")
(require "treinreeks-adt.rkt")
(require "wissel-adt.rkt")
(require "spoornetwerk-adt.rkt")
(require "../GUI.rkt")

(require "a-d/graph/unweighted/adjacency-matrix.rkt")
(require "a-d/graph-algorithms/undirected/bft-applications.rkt")

(provide maak-nmbs)

(define SNELHEIDSVERANDERING 20)

;; Dit draait op onze computer zelf
(define (maak-nmbs)
  (let ((spoor (maak-spoornetwerk)))


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
      ((spoor 'wijzig-stand-switch!) id stand))

    (define (dispatch-nmbs msg . args)
      (cond ((eq? msg 'zet-trein-op-spoor) (zet-trein-op-spoor! (car args) (cadr args) (caddr args)))
            ((eq? msg 'verhoog-snelheid-trein!) (verhoog-snelheid-trein! (car args)))
            ((eq? msg 'verlaag-snelheid-trein!) (verlaag-snelheid-trein! (car args)))
            ((eq? msg 'geef-snelheid-trein) (geef-snelheid-trein (car args)))
            ((eq? msg 'bereken-traject) (bereken-traject (car args) (cadr args)))
            ((eq? msg 'geef-wissel-ids) (geef-wissel-ids))
            ((eq? msg 'geef-aanwezige-treinen) (geef-aanwezige-treinen))
            ((eq? msg 'geef-trein-ids) (geef-trein-ids))
            ((eq? msg 'geef-detectieblok-ids) (geef-detectieblok-ids))
            ((eq? msg 'verander-wisselstand!) (verander-wisselstand! (car args) (cadr args)))
            (else (display "foute boodschap - NMBS"))))

    (define GUI (maak-gui dispatch-nmbs))
    (GUI 'start)
    (GUI 'teken-wissel-panel (geef-wissel-ids))
    (GUI 'teken-detectieblok-panel (geef-detectieblok-ids))
    
    dispatch-nmbs))