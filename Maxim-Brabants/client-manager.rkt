#lang racket


;;; Racket file for managing all the NMBS clients
;;; all the created clients will be kept here
;;; this manager will make sure all the clients will stay synchronized

(provide maak-client-manager)

(define (maak-client-manager)
  (let ((active-clients '()))

    ;; wordt opgeroepen voor de eerste client die wordt aangemaakt (main)
    (define (set-first-client! nmbs-client)
      (set! active-clients (cons nmbs-client active-clients)))
    
    ;; alle aanwezige treinen tekenen in GUI van nieuwe client
    (define (draw-trains nmbs-client train-ids)
      (let iter-ids
        ((ids train-ids))
        (when (not (null? ids))
          ((nmbs-client 'GUI) 'teken-trein-in-panel (symbol->string (car ids)))
          (iter-ids (cdr ids)))))

    ;; nieuwe trein aan de GUI van alle clients toevoegen
    (define (synchronize-new-train client-id train-id)
      (let client-iter
        ((clients active-clients))
        (when (and (not (null? clients))
                   (not (= client-id ((car clients) 'client-id))))
          (((car clients) 'GUI) 'teken-trein-in-panel (symbol->string train-id))
          (client-iter (cdr clients)))))

    ;; verandering van wisselstand synchroniseren met alle clients
    (define (synchronize-switch-state client-id switch-id selection)
      (let client-iter
        ((clients active-clients))
        (when (and (not (null? clients))
                   (not (= client-id ((car clients) 'client-id))))
          (((car clients) 'GUI) 'teken-wissel-status switch-id selection)
          (client-iter (cdr clients)))))

    ;; verandering van wisselstand synchroniseren met alle clients
    (define (synchronize-change-speed client-id train-id speed)
      (let client-iter
        ((clients active-clients))
        (when (and (not (null? clients))
                   (not (= client-id ((car clients) 'client-id))))
          (((car clients) 'GUI) 'teken-trein-snelheid train-id (number->string speed))
          (client-iter (cdr clients)))))
          
    
    (define (add-new-client nmbs-client train-ids switch-selections trein-snelheden)
      (set! active-clients (cons nmbs-client active-clients))
      (draw-trains nmbs-client (reverse train-ids))
      ;; wisselstanden synchroniseren met nieuwe client
      (let selection-iter
        ((switches switch-selections))
        (when (not (null? switches))
          ((nmbs-client 'GUI) 'teken-wissel-status (caar switches) (cdar switches))
          (selection-iter (cdr switches))))
      ;; treinsnelheden synchroniseren met nieuwe client
      (let speed-iter
        ((speeds trein-snelheden)
         (ids train-ids))
        (when (and (not (null? speeds)) (not (null? ids)))
          ((nmbs-client 'GUI) 'teken-trein-snelheid (car ids) (number->string (car speeds)))
          (speed-iter (cdr speeds) (cdr ids)))))


    (define (dispatch-manager msg . args)
      (cond ((eq? msg 'active-clients) active-clients)
            ((eq? msg 'set-first-client!) (set-first-client! (car args)))
            ((eq? msg 'add-new-client) (add-new-client (car args) (cadr args) (caddr args) (cadddr args)))
            ((eq? msg 'synchronize-new-train) (synchronize-new-train (car args) (cadr args)))
            ((eq? msg 'synchronize-switch-state) (synchronize-switch-state (car args) (cadr args) (caddr args)))
            ((eq? msg 'synchronize-change-speed) (synchronize-change-speed (car args) (cadr args) (caddr args)))
            (else (display "wrong-message-client-manager"))))
    dispatch-manager))