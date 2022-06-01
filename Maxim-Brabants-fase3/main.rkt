#lang racket

(require "NMBS/NMBS.rkt")
(require "client-manager.rkt")

(define client-manager (maak-client-manager))
(define nmbs (maak-nmbs client-manager))
(client-manager 'set-first-client! nmbs)

(nmbs 'zet-trein-op-spoor! "T-1" "D5" "D6")
(nmbs 'zet-trein-op-spoor! "T-2" "D6" "D9")
