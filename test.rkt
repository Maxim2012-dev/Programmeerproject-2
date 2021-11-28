#lang racket

(require "simulator/interface.rkt")
(require "trein-adt.rkt")
(require "treinreeks-adt.rkt")
(require "wissel-adt.rkt")
(require "spoornetwerk-adt.rkt")


(setup-loop)

(define snelheidstrein (maak-trein 'T-1 'S-27 '1-3))
(start)