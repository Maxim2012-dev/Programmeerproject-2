#lang racket

(provide request-rail-network
         send-train-message
         send-change-switch
         send-new-client
         send-rail-network)


;;; API for connection between NMBS & INFRABEL
;;; all messsages sent will go through here
;;; this API will make sure all the tcp-traffic is forwarded to the right output port



;;; Operations intented for INFRABEL
;;; (API always passes a tag for INFRABEL to know what to do with the input (assumption))
;;; =======================================================================================
(define (request-rail-network out)
  (write (list 'rail-network) out)
  (flush-output out))

(define (send-train-message train out)
  (when train
    (write (list 'new-train train) out)
    (flush-output out)))

(define (send-change-switch id status out)
  (when (or (= status 1) (= status 2))
    (write (list 'switch-status id status) out)
    (flush-output out)
    (display "switch-change-tcp")))

(define (send-new-client nmbs-client out)
  (when nmbs-client
    (write (list 'new-client nmbs-client) out)
    (flush-output out)))


;;; Operations intented for NMBS
;;; =======================================================================================
(define (send-rail-network out)
  (write (list 'rail-network) out)
  (flush-output out)
  (display "sent"))