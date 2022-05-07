#lang r6rs
(import (rnrs base)
        (a-d sat cnf)
        (a-d sat interpretation)
        (a-d sat dimacs-parser)
        (a-d sat logger)
        (prefix (a-d sat cdcl standard) standard:)
        (a-d sat cdcl minimizing))

(define test-file "cnf-samples/example-mini.cnf")

(log-set-verbosity! 'warning)
(define formula (parse-dimacs-file test-file))

(log-debug "Checking satisfiability for formula:\n" (formula->string formula))

;; Standard CDCL
(define res (standard:cdcl formula))
(log-warning "The standard CDCL result is:" (if res (interpret->string res) res))

;; Minimizing CDCL
(set! formula (parse-dimacs-file test-file)) ;; always reset the formula when you want to run another algorithm!
(define res2 (cdcl formula))
(log-warning "The minimizing CDCL result is:" (if res2 (interpret->string res2) res2))