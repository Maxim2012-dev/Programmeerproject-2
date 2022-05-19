#lang racket

(provide send-train-message)

;;; API for connection between NMBS & INFRABEL
;;; all messsages sent will go through here
;;; this API will make sure all the tcp-traffic is forwarded to the right output port



(define (send-train-message train output-port)
  (write train output-port)
  (flush-output output-port)
  (display "works"))