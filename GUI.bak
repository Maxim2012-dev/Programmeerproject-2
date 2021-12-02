#lang racket

(require racket/gui/base)


(define window (new frame%
                    [label "User Interface"]
                    [width 600]
                    [height 600]))

(define tab-panel (new tab-panel%
                       [choices (list "Functionaliteit"
                                      "Spoornetwerk"
                                      "Logboek")]
                       [parent window]))
(send window show #t)

