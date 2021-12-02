#lang racket

(require racket/gui/base)

(define FUNCTIONALITY_TAB_LABEL "Info spoornetwerk:")

(define window (new frame%
                    [label "User Interface"]
                    [width 600]
                    [height 600]))


(define (change-tab tp event)
  (when (eq? (send event get-event-type) 'tab-panel)
    (fill-tab-content tp)))

(define tab-panel (new tab-panel%
                       [choices (list "Functionaliteit"
                                      "Spoornetwerk"
                                      "Logboek")]
                       [callback change-tab]
                       [parent window]))

; Layout voor het tablad 'Functionaliteit'
(define (tab-functionaliteit children-areas)
  (list
   (new vertical-panel%	 
        [parent tab-panel]	 
        [style (list 'border)])))

; Layout voor het tablad 'Spoornetwerk'
(define (tab-spoornetwerk children-areas)
  (list
   (new message%
        [label FUNCTIONALITY_TAB_LABEL]
        [parent tab-panel]
        [vert-margin 25])
   (new vertical-panel%	 
        [parent tab-panel]	 
        [style (list 'border)])))

; Layout voor het tablad 'Logboek'
(define (tab-logboek children-areas)
  (list
   (new vertical-panel%	 
        [parent tab-panel]	 
        [style (list 'border)])))

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
    

(send window show #t)

