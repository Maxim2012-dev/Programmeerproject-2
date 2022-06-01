(load "../examples/complex-2.ss")

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                         complex tests                           *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universitent Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define cpx1 (make-complex 1 4))
(define cpx2 (make-complex 5 3))
(display ((cpx1 '+ cpx2) 'complex->list))(newline)
(display ((cpx1 '* cpx2) 'complex->list))(newline)
(display ((cpx1 '- cpx2) 'complex->list))(newline)
(display ((cpx1 '/ cpx2) 'complex->list))(newline)

(display (cpx1 'real))(newline)
(display (cpx2 'imag))(newline)
(display (cpx1 'modulus))(newline)
(display (cpx2 'argument))