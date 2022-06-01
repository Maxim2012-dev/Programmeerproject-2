#lang racket

(require racket/gui/base)

(provide maak-gui)

;; ======================= STRING CONSTANTS =======================

(define TRAINS_VIEW "Treinoverzicht")
(define SWITCHES_VIEW "Wisselsoverzicht")
(define DETECTION_BLOCKS_VIEW "Detectieblokkenoverzicht")
(define TRAIN_ID_LABEL "Trein met ID:   ")
(define SWITCH_ID_LABEL "Switch met ID:   ")
(define DETECTION_BLOCK_ID_LABEL "Detectieblok met ID:   ")
(define ADD_TRAIN_LABEL "Voeg trein toe")
(define EMPTY "leeg")
(define NEW_TRAIN_TITLE "Nieuwe trein")
(define AUTOMATIC_ROUTE "Automatisch traject")
(define CHOOSE_FORMATION "Opstelling kiezen")
(define ADD_CLIENT "Client toevoegen")
(define CUSTOM_FORMATION "Geef bestand met opstelling: ")
(define FORMATION "Kies een opstelling")
(define ADDED_TRAIN_TO_TRACK "Nieuwe trein toegevoegd aan spoor")
(define SPEED_TRAIN_INCREASED "Snelheid verhoogd van trein met ID: ")
(define SPEED_TRAIN_DECREASED "Snelheid verlaagd van trein met ID: ")
(define SWITCH_CHANGED " veranderd naar stand: ")
(define INCREASE_SPEED "+ Snelheid")
(define DECREASE_SPEED "- Snelheid")


(define (maak-gui nmbs)
  (let ((status-list '())
        (train-speed-labels '())
        (radio-box-list '()))
  
    (define window (new frame%
                        [label "User Interface"]
                        [width 650]
                        [height 700]))


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
                              [style (list 'border 'auto-vscroll)]))

    ; Panel om wissels met hun stand in weer te geven
    (define switches-panel (new vertical-panel%	 
                                [parent tab-panel]	 
                                [style (list 'border 'auto-vscroll)]))

    ; Panel om detectieblokken met hun status in weer te geven
    (define detection-blocks-panel (new vertical-panel%	 
                                        [parent tab-panel]	 
                                        [style (list 'border 'auto-vscroll)]))

    ; Panel om gebeurtenissen in te loggen
    (define log-panel (new vertical-panel%
                           [parent tab-panel]
                           [style (list 'border 'auto-vscroll)]))



    ; Wanneer op een radiobutton wordt geklikt (switches)
    (define (onRadioClick radiobtn event switch-id)
      (let ((selection (send radiobtn get-selection))
            (switch-id-string (symbol->string switch-id)))
        (when selection
          (cond ((= selection 0)
                 (nmbs 'verander-wisselstand! selection switch-id 1)
                 (new message%
                      [label (string-append SWITCH_ID_LABEL switch-id-string SWITCH_CHANGED (send radiobtn get-item-label 0))]
                      [parent log-panel]))
                ((= selection 1)
                 (nmbs 'verander-wisselstand! selection switch-id 2)
                 (new message%
                      [label (string-append SWITCH_ID_LABEL switch-id-string SWITCH_CHANGED (send radiobtn get-item-label 1))]
                      [parent log-panel]))))))

    (define (get-selections-switches)
      (reverse (map (lambda (el) (cons (car el)(send (cdr el) get-selection)))
                    radio-box-list)))
    
    
    (define (teken-wissel-status id selection)
      (when (not (null? radio-box-list))
        (let ((radio-box (cdr (assoc id radio-box-list))))
          (send radio-box set-selection selection))))

    ; Het panel met de wissels vullen
    (define (teken-wissel-panel)
      (let ((ids (nmbs 'geef-wissel-ids)))
        (define (iter ids)
          (when (not (null? ids))
            (define panel (new horizontal-panel% [parent switches-panel]
                               [alignment '(center top)]))
            (new message% 
                 [label (string-append SWITCH_ID_LABEL (symbol->string (car ids)) "\t")]
                 [parent panel])
            (set! radio-box-list
                  (cons (cons (car ids)(new radio-box% [label "stand: "][choices (list "1" "2")][parent panel]
                                                           [style (list 'horizontal)]
                                                           [callback (lambda (r e) (onRadioClick r e (car ids)))])) radio-box-list))
            (iter (cdr ids))))
        (iter ids)))

    ; Het panel met de detectieblokken vullen
    (define (teken-detectieblok-panel)
      (let ((ids (nmbs 'geef-detectieblok-ids)))
        (define (iter ids)
          (when (not (null? ids))
            (define panel (new horizontal-panel% [parent detection-blocks-panel]
                               [alignment '(center top)]))
            (new message% 
                 [label (string-append DETECTION_BLOCK_ID_LABEL (symbol->string (car ids)) "\t --------> \t")]
                 [parent panel])
            (set! status-list (cons (cons (car ids)(new message% 
                                                        [label EMPTY]
                                                        [parent panel])) status-list))
            (send destination-list append (symbol->string (car ids)) (car ids))
            (iter (cdr ids))))
        (iter ids)
        (set! status-list (reverse status-list))))
    

    (define (teken-detectieblok-status id block)
      (when (not (null? status-list))
        (let ((status-label (cdr (assoc block status-list))))
          (send status-label set-label (symbol->string id)))))
    
    (define (reset-statuses)
      (let iter
        ((list status-list))
        (when (not (null? list))
          (send (cdar list) set-label EMPTY)
          (iter (cdr list)))))

    ; Statussen van de detectieblokken verversen
    (define (refresh-detection-blocks)
      (reset-statuses)
      ; over alle treinen gaan
      (let trains-iter
        ((trains ((nmbs 'geef-aanwezige-treinen) 'reeks)))
        (when (not (null? trains))
          (nmbs 'detectieblok-trein ((car trains) 'trein-id))
          (trains-iter (cdr trains))))
      (sleep 0.5)
      (refresh-detection-blocks))
    (thread refresh-detection-blocks)

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
    (define cancelBtn (new button% [parent panel] [label "Annuleer"]
                           [callback (lambda (b e) (send new-train-dialog show #f))]))
    (define confirmBtn (new button% [parent panel] [label "Bevestig"]
                            [callback (lambda (b e)(onClickConfirm (send id get-value)
                                                                   (send direction get-value)
                                                                   (send segment get-value))
                                        (send new-train-dialog show #f))]))


    ; ======================= AUTOMATISCHE ROUTE STARTEN =======================

    (define (onClickStartRoute btn event)
      (let ((train-id (send train-list get-data
                            (car (send train-list get-selections))))
            (destination (send destination-list get-data
                               (car (send destination-list get-selections)))))
        (send auto-route-dialog show #f)
        (nmbs 'bereken-traject train-id destination)))

    ; ======================= DIALOG - AUTOMATISCHE ROUTE =======================

    (define auto-route-dialog (new dialog%	 
                                   [label AUTOMATIC_ROUTE]
                                   [parent window]
                                   [width 400]	 
                                   [height 300]))

    (define train-list (new list-box%
                            [label "Trein: "]
                            [choices '()]
                            [parent auto-route-dialog]))

    (define destination-list (new list-box%
                                  [label "Eindbestemming: "]
                                  [choices '()]
                                  [parent auto-route-dialog]))

    (define start-route-btn (new button% [parent auto-route-dialog] [label "Start traject"]
                                 [callback onClickStartRoute]))

    ; ======================= TREINEN TOEVOEGEN =======================

    ; Nieuwe trein aan Treinoverzicht toevoegen
    (define (add-to-trains-panel train-id)
      (define panel (new horizontal-panel% [parent trains-panel]
                         [alignment '(center top)]))
      (new message% 
           [label (string-append TRAIN_ID_LABEL "  " train-id "\t\t")]
           [parent panel])
      (new button% [parent panel] [label INCREASE_SPEED]
           [callback (lambda (b e)(new message%
                                       [label (string-append SPEED_TRAIN_INCREASED train-id)]
                                       [parent log-panel])
                       (nmbs 'verhoog-snelheid-trein! train-id))])
      (define speed (new message%
                         [label "0"]
                         [parent panel]
                         [horiz-margin 50]
                         [auto-resize #t]))
      (set! train-speed-labels (cons (cons (string->symbol train-id) speed) train-speed-labels))
      (new button% [parent panel] [label DECREASE_SPEED]
           [callback (lambda (b e)(new message%
                                       [label (string-append SPEED_TRAIN_DECREASED train-id)]
                                       [parent log-panel])
                       (nmbs 'verlaag-snelheid-trein! train-id))]))

    (define (onClickConfirm id direction segment)
      (new message%
           [label ADDED_TRAIN_TO_TRACK]
           [parent log-panel])
      (send train-list append id (string->symbol id))
      (nmbs 'zet-trein-op-spoor! id direction segment))

    (define (teken-trein-snelheid trein-id snelheid)
      (let ((speed-label (cdr (assoc trein-id train-speed-labels))))
        (send speed-label set-label snelheid)))


    ; ======================= DIALOG - OPSTELLING KIEZEN =======================

    (define choose-formation-dialog (new dialog%	 
                                         [label CHOOSE_FORMATION]
                                         [parent window]
                                         [width 300]	 
                                         [height 250]))

    (define choose-formation (new message%
                                  [label FORMATION]
                                  [parent choose-formation-dialog]
                                  [min-width 100]
                                  [min-height 40]))

    (define custom-formation (new text-field% [parent choose-formation-dialog] [label CUSTOM_FORMATION]))
    (define confirm (new button% [parent choose-formation-dialog] [label "Bevestig"]
                         [callback (lambda (b e) (nmbs 'lees-opstelling-uit (send custom-formation get-value)))]))

    ;; ======================= TAB LAYOUTS =======================


    ; Layout voor het tablad 'Functionaliteit'
    (define (tab-functionaliteit children-areas)
      (list
       (new button%
            [label ADD_TRAIN_LABEL]
            [parent tab-panel]
            [callback (lambda (b e) (send new-train-dialog show #t))])
       (new button%
            [label AUTOMATIC_ROUTE]
            [parent tab-panel]
            [callback (lambda (b e) (send auto-route-dialog show #t))])
       (new button%
            [label CHOOSE_FORMATION]
            [parent tab-panel]
            [callback (lambda (b e) (send choose-formation-dialog show #t))])
       (new button%
            [label ADD_CLIENT]
            [parent tab-panel]
            [callback (lambda (b e) (nmbs 'voeg-nieuwe-client-toe))])))

    ; Layout voor het tablad 'Spoornetwerk'
    (define (tab-overzicht children-areas)
      (list
       (new message%
            [label TRAINS_VIEW]
            [parent tab-panel]
            [vert-margin 10])
       trains-panel
       (new message%
            [label SWITCHES_VIEW]
            [parent tab-panel]
            [vert-margin 10])
       switches-panel
       (new message%
            [label DETECTION_BLOCKS_VIEW]
            [parent tab-panel]
            [vert-margin 10])
       detection-blocks-panel))

    ; Layout voor het tablad 'Logboek'
    (define (tab-logboek children-areas)
      (list
       log-panel))

    
    ; toont venster met alle inhoud
    (define (start-gui)
      (send window show #t)
      (fill-tab-content tab-panel))

    (define (dispatch-gui msg . args)
      (cond ((eq? msg 'start) (start-gui))
            ((eq? msg 'teken-wissel-panel) (teken-wissel-panel))
            ((eq? msg 'teken-detectieblok-panel) (teken-detectieblok-panel))
            ((eq? msg 'refresh-detection-blocks) refresh-detection-blocks)
            ((eq? msg 'teken-trein-in-panel) (add-to-trains-panel (car args)))
            ((eq? msg 'teken-trein-snelheid) (teken-trein-snelheid (car args) (cadr args)))
            ((eq? msg 'teken-detectieblok-status) (teken-detectieblok-status (car args) (cadr args)))
            ((eq? msg 'teken-wissel-status) (teken-wissel-status (car args) (cadr args)))
            ((eq? msg 'get-selections-switches) (get-selections-switches))
            (else (display "wrong message gui"))))
    dispatch-gui))





