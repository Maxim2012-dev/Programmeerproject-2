#lang racket

(provide request-switch-ids
         request-detection-block-ids
         request-train-speed
         request-loco-detection-block
         send-train-message
         send-change-switch
         send-change-train-speed
         send-switch-ids
         send-detection-block-ids
         send-draw-train
         send-draw-train-speed
         send-draw-loco-block)


;;; API for connection between NMBS & INFRABEL
;;; all messsages sent will go through here
;;; this API will make sure all the tcp-traffic is forwarded to the right output port



;;; Operations intented for INFRABEL
;;; (API always passes a tag for INFRABEL to know what to do with the input (assumption))
;;; =======================================================================================

(define (request-switch-ids out)
  (write (list 'switch-ids) out)
  (flush-output out))

(define (request-detection-block-ids out)
  (write (list 'detection-block-ids) out)
  (flush-output out))

(define (request-train-speed id out)
  (when (symbol? id)
    (write (list 'train-speed) out)
    (flush-output out)))

(define (request-loco-detection-block id out)
  (when (symbol? id)
    (write (list 'loco-block id) out)
    (flush-output out)))

(define (send-train-message id direction segment out)
  (when (and (symbol? id)
             (symbol? direction)
             (symbol? segment))
    (write (list 'new-train id direction segment) out)
    (flush-output out)))

(define (send-change-switch id status out)
  (when (or (= status 1) (= status 2))
    (write (list 'switch-status id (number->string status)) out)
    (flush-output out)))

(define (send-change-train-speed id action out)
  (when (and (symbol? id) (symbol? action))
    (write (list 'change-speed id action) out)
    (flush-output out)))



;;; Operations intented for NMBS
;;; (API always passes a tag for NMBS to know what to do with the input (assumption))
;;; =======================================================================================

(define (send-switch-ids list out)
  (when (list? list)
    (write (cons 'switch-ids list) out)
    (flush-output out)))

(define (send-detection-block-ids list out)
  (when (list? list)
    (write (cons 'detection-block-ids list) out)
    (flush-output out)))

(define (send-draw-train id out)
  (if (symbol? id)
      (write (list 'draw-train id) out)
      (write (list 'draw-train "draw-train-error-tcp")))
  (flush-output out))

(define (send-draw-train-speed id speed out)
  (when (symbol? id)
    (write (list 'draw-train-speed id (number->string speed)) out)
    (flush-output out)))

(define (send-draw-loco-block id block out)
  (when (and (symbol? id) (symbol? block))
    (write (list 'draw-loco-block id block) out)
    (flush-output out)))







  