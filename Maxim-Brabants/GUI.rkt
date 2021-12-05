#lang racket

(require racket/gui/base)
(require "simulator/interface.rkt")
(require "INFRABEL.rkt")

;; ======================= STRING CONSTANTS =======================

(define TRAINS_VIEW "Treinoverzicht")
(define SWITCHES_VIEW "Wisselsoverzicht")
(define DETECTION_BLOCKS_VIEW "Detectieblokkenoverzicht")
(define TRAIN_ID_LABEL "Trein met ID:   ")
(define SWITCH_ID_LABEL "Switch met ID:   ")
(define DETECTION_BLOCK_ID_LABEL "Detectieblok met ID:   ")
(define ADD_TRAIN_LABEL "Voeg trein toe")
(define NEW_TRAIN_TITLE "Nieuwe trein")
(define ADDED_TRAIN_TO_TRACK "Nieuwe trein toegevoegd aan spoor")
(define SPEED_TRAIN_INCREASED "Snelheid verhoogd van trein met ID: ")
(define SPEED_TRAIN_DECREASED "Snelheid verlaagd van trein met ID: ")
(define SWITCH_CHANGED " veranderd naar stand: ")
(define INCREASE_SPEED "+ Snelheid")
(define DECREASE_SPEED "- Snelheid")



(define window (new frame%
                    [label "User Interface"]
                    [width 600]
                    [height 600]))


(define (change-tab tp event)
  (when (eq? (send event get-event-type) 'tab-panel)
    (fill-tab-content tp)))

(define (fill-tab-content tp)
  (define current-tab-name
    (send tp get-item-label (send tp get-selection)))
  (send tp change-children
        (cond ((eq? current-tab-name "Functionaliteit")
               tab-functionaliteit)
              ((eq? current-tab-name "Overzicht")
               tab-overzicht)
              ((eq? current-tab-name "Logboek")
               tab-logboek)
              (else (display "error - tab doesn't exist")))))

(define tab-panel (new tab-panel%
                       [parent window]
                       [choices (list "Functionaliteit"
                                      "Overzicht"
                                      "Logboek")]
                       [callback change-tab]))


; ======================= CONTENT PANELS =======================

; Panel om alle treinen en hun details in weer te geven
(define trains-panel (new vertical-panel%	 
                          [parent tab-panel]	 
                          [style (list 'border)]))

; Panel om wissels met hun stand in weer te geven
(define switches-panel (new vertical-panel%	 
                            [parent tab-panel]	 
                            [style (list 'border)]))

; Panel om detectieblokken met hun status in weer te geven
(define detection-blocks-panel (new vertical-panel%	 
                                    [parent tab-panel]	 
                                    [style (list 'border)]))

; Panel om gebeurtenissen in te loggen
(define log-panel (new vertical-panel%
                       [parent tab-panel]
                       [style (list 'border)]))


; Nieuwe trein aan Treinoverzicht toevoegen
(define (add-to-trains-panel train-id)
  (define panel (new horizontal-panel% [parent trains-panel]
                     [alignment '(center top)]))
  (new message% 
       [label (string-append TRAIN_ID_LABEL train-id)]
       [parent panel])
  (new button% [parent panel] [label INCREASE_SPEED]
       [callback (lambda (b e)(new message%
                                   [label (string-append SPEED_TRAIN_INCREASED train-id)]
                                   [parent log-panel])
                   ((infrabel 'verhoog-snelheid-trein!) train-id)
                   (send speed set-label (number->string ((infrabel 'geef-snelheid-trein) train-id))))])
  (define speed (new message%
                     [label (number->string ((infrabel 'geef-snelheid-trein) train-id))]
                     [parent panel]
                     [horiz-margin 50]
                     [auto-resize #t]))
  (new button% [parent panel] [label DECREASE_SPEED]
       [callback (lambda (b e)(new message%
                                   [label (string-append SPEED_TRAIN_DECREASED train-id)]
                                   [parent log-panel])
                   ((infrabel 'verlaag-snelheid-trein!) train-id)
                   (send speed set-label (number->string ((infrabel 'geef-snelheid-trein) train-id))))]))


; Wanneer op een radiobutton wordt geklikt
(define (onRadioClick radiobtn event switch-id)
  (let ((selection (send radiobtn get-selection))
        (switch-id-symbol (symbol->string switch-id)))
    (when selection
      (cond ((= selection 0)
             ((infrabel 'verander-wisselstand!) switch-id-symbol 1)
             (new message%
                  [label (string-append SWITCH_ID_LABEL switch-id-symbol SWITCH_CHANGED (send radiobtn get-item-label 0))]
                  [parent log-panel]))
            ((= selection 1)
             ((infrabel 'verander-wisselstand!) switch-id-symbol 2)
             (new message%
                  [label (string-append SWITCH_ID_LABEL switch-id-symbol SWITCH_CHANGED (send radiobtn get-item-label 1))]
                  [parent log-panel]))))))

; Het panel met de wissels vullen
(define (fill-switches-panel)
  (let ((switch-ids ((infrabel 'geef-wissel-ids))))
    (define (iter ids)
      (when (not (null? ids))
        (define panel (new horizontal-panel% [parent switches-panel]
                           [alignment '(center top)]))
        (new message% 
             [label (string-append SWITCH_ID_LABEL (symbol->string (car ids)) "\t")]
             [parent panel])
        (new radio-box% [label "stand: "][choices (list "1" "2")][parent panel]
             [style (list 'horizontal)]
             [callback (lambda (r e) (onRadioClick r e (car ids)))])
        (iter (cdr ids))))
    (iter switch-ids)))

; Het panel met de detectieblokken vullen
(define (fill-detection-blocks-panel)
  (let ((detection-block-ids ((infrabel 'geef-detectieblok-ids))))
    (define (iter ids)
      (when (not (null? ids))
        (define panel (new horizontal-panel% [parent detection-blocks-panel]
                           [alignment '(center top)]))
        (new message% 
             [label (string-append DETECTION_BLOCK_ID_LABEL (symbol->string (car ids)))]
             [parent panel])
        (iter (cdr ids))))
    (iter detection-block-ids)))

; ======================= DIALOG - NIEUWE TREIN =======================

(define new-train-dialog (new dialog%	 
                              [label NEW_TRAIN_TITLE]	 
                              [parent window]
                              [width 300]	 
                              [height 300]))

; Tekstvelden
(define id (new text-field% [parent new-train-dialog] [label "Trein-ID: "]))
(define direction (new text-field% [parent new-train-dialog] [label "Richting: "]))
(define segment (new text-field% [parent new-train-dialog] [label "Huidig segment: "]))
; Om de knoppen te centreren
(define panel (new horizontal-panel% [parent new-train-dialog]
                   [alignment '(center center)]))
; Twee knoppen
(new button% [parent panel] [label "Annuleer"]
     [callback (lambda (b e) (send new-train-dialog show #f))])
(new button% [parent panel] [label "Bevestig"]
     [callback (lambda (b e)(onClickConfirm (send id get-value)
                                            (send direction get-value)
                                            (send segment get-value))
                 (add-to-trains-panel (send id get-value))
                 (send new-train-dialog show #f))])


; ======================= TREINEN TOEVOEGEN =======================

(define (onClickConfirm id direction segment)
  (new message%
       [label ADDED_TRAIN_TO_TRACK]
       [parent log-panel])
  ((infrabel 'zet-trein-op-spoor) id direction segment))

(define (onClickAddTrain btn event)
  (send new-train-dialog show #t))


;; ======================= TAB LAYOUTS =======================


; Layout voor het tablad 'Functionaliteit'
(define (tab-functionaliteit children-areas)
  (list
   (new button%
        [label ADD_TRAIN_LABEL]
        [parent tab-panel]
        [callback onClickAddTrain])))

; Layout voor het tablad 'Spoornetwerk'
(define (tab-overzicht children-areas)
  (list
   (new message%
        [label TRAINS_VIEW]
        [parent tab-panel]
        [vert-margin 20])
   trains-panel
   (new message%
        [label SWITCHES_VIEW]
        [parent tab-panel]
        [vert-margin 20])
   switches-panel
   (new message%
        [label DETECTION_BLOCKS_VIEW]
        [parent tab-panel]
        [vert-margin 20])
   detection-blocks-panel))

; Layout voor het tablad 'Logboek'
(define (tab-logboek children-areas)
  (list
   log-panel))


;; ===============================================================
;; ======================= START PROGRAMMA =======================
;; ===============================================================

(define infrabel (maak-programma))
((infrabel 'start-programma) setup-loop-and-switches)

(send window show #t)
(fill-tab-content tab-panel)
(fill-switches-panel)
(fill-detection-blocks-panel)
