#lang racket

(require "NMBS/NMBS.rkt")
(require "simulator/interface.rkt")


(setup-loop-and-switches)
(start)

(define nmbs (maak-nmbs))
