#lang racket


;;; Racket file for managing all the NMBS clients
;;; all the created clients will be kept here
;;; this manager will make sure all the clients will stay synchronized

(provide maak-client-manager)

(define (maak-client-manager)
  (let ((active-clients '()))

    (define (add-new-client nmbs-client)
      (set! active-clients (cons nmbs-client active-clients))
      (displayln "added client"))


    (define (dispatch-manager msg . args)
      (cond ((eq? msg 'active-clients) active-clients)
            ((eq? msg 'add-new-client) (add-new-client (car args)))
            (else (display "wrong-message-client-manager"))))
    dispatch-manager))