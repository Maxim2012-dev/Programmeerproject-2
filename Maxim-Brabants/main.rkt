#lang racket

(require "NMBS/NMBS.rkt")
(require "simulator/interface.rkt")


(setup-loop-and-switches)
(start)

(define nmbs (maak-nmbs))

(nmbs 'zet-trein-op-spoor! "T-1" "D5" "D6")
(nmbs 'zet-trein-op-spoor! "T-2" "D5" "D9")
(nmbs 'zet-trein-op-spoor! "T-3" "D5" "D1")
