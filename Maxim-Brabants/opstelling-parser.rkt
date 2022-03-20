#lang racket

(require "simulator/railway.rkt")
(provide start-parser)


(define (read-formation file components)
  (define size (vector-length components))
  (define connections '())
  ;; componenten + index in vector
  (define (read-components file size)
    (unless (<= size 0)
      (let* ((index (read file))
             (component (read file)))
        (display index) (display " ") (display component)(newline)
        (vector-set! components index component)
        (read-components file (- size 1)))))
  ;; relaties tussen componenten
  (define (read-relations file)
    (let ((from (read file))
          (to (read file)))
      (unless (eof-object? from)
        (display from)(display " verbonden met ")(display to)(newline)
        (set! connections (cons (cons from to) connections))
        (read-relations file))))
  (read-components file size)
  (read-relations file)
  (load-custom-setup components connections))

;; ontvangt naam van tekstbestand en opent een filestream ermee
;; + start uitlezen van opstelling
(define (start-parser filename)
  (define in (open-input-file (string-append filename ".txt")))
  ;; aantal componenten inlezen en gebruiken als vector-size
  (define components (make-vector (read in)))
  (read-formation in components)
  (close-input-port in))

