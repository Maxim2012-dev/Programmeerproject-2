#lang racket

(require racket/gui/base)
(require "simulator/interface.rkt")
(require "INFRABEL.rkt")

(define RAIL_NETWORK_TAB_LABEL "Info spoornetwerk:")
(define ADD_TRAIN_LABEL "Voeg trein toe")
(define NEW_TRAIN_TITLE "Nieuwe trein")



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
              ((eq? current-tab-name "Spoornetwerk")
               tab-spoornetwerk)
              ((eq? current-tab-name "Logboek")
               tab-logboek)
              (else (display "error - tab doesn't exist")))))

(define tab-panel (new tab-panel%
                       [parent window]
                       [choices (list "Functionaliteit"
                                      "Spoornetwerk"
                                      "Logboek")]
                       [callback change-tab]))


; Panel om alle treinen in weer te geven
(define trains-panel (new vertical-panel%	 
                          [parent tab-panel]	 
                          [style (list 'border)]))

; ======================= TREINEN TOEVOEGEN =======================

(define (onClickConfirm id direction segment)
  ((infrabel 'zet-trein-op-spoor) id direction segment))

(define (onClickAddTrain btn event)
  (define new_train (new message%
                         [label "new_train"]
                         [parent trains-panel]))
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
  (new button% [parent panel] [label "Ok"]
       [callback (lambda (b e) (onClickConfirm (send id get-value)
                                               (send direction get-value)
                                               (send segment get-value)))])

  (send trains-panel after-new-child new_train)
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
(define (tab-spoornetwerk children-areas)
  (list
   (new message%
        [label RAIL_NETWORK_TAB_LABEL]
        [parent tab-panel]
        [vert-margin 25])
   trains-panel))

; Layout voor het tablad 'Logboek'
(define (tab-logboek children-areas)
  (list
   (new vertical-panel%	 
        [parent tab-panel]	 
        [style (list 'border)])))



;; ======================= START PROGRAMMA =======================

(define infrabel (maak-programma))
((infrabel 'start-programma) setup-loop)

(send window show #t)

