#lang racket


;;; Racket file for managing all the NMBS clients
;;; all the created clients will be kept here
;;; this manager will make sure all the clients will stay synchronized

(provide maak-client-manager)

(define (maak-client-manager)
  (let ((active-clients '()))

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
          
    
    (define (add-new-client nmbs-client train-ids switch-selections)
      (set! active-clients (cons nmbs-client active-clients))
      (let selection-iter
        ((switches switch-selections))
        (when (not (null? switches))
          ((nmbs-client 'GUI) 'teken-wissel-status (caar switches) (cdar switches))
          (selection-iter (cdr switches))))
      (draw-trains nmbs-client (reverse train-ids)))


    (define (dispatch-manager msg . args)
      (cond ((eq? msg 'active-clients) active-clients)
            ((eq? msg 'add-new-client) (add-new-client (car args) (cadr args) (caddr args)))
            ((eq? msg 'synchronize-new-train) (synchronize-new-train (car args) (cadr args)))
            ((eq? msg 'synchronize-switch-state) (synchronize-switch-state (car args) (cadr args) (caddr args)))
            (else (display "wrong-message-client-manager"))))
    dispatch-manager))