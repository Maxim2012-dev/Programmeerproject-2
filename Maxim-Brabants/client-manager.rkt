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
    
    (define (add-new-client nmbs-client train-ids)
      (set! active-clients (cons nmbs-client active-clients))
      (draw-trains nmbs-client train-ids))


    (define (dispatch-manager msg . args)
      (cond ((eq? msg 'active-clients) active-clients)
            ((eq? msg 'add-new-client) (displayln args)(add-new-client (car args) (cadr args)))
            (else (display "wrong-message-client-manager"))))
    dispatch-manager))