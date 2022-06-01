#lang racket

(provide start-parser)


(define (read-formation file components)
  (define size (vector-length components))
  ;; bevat de piko treinspoorstukken (vector van lijsten en/of atomen)
  (define piko-tracks (make-vector size))
  (define connections '())
  ;; componenten + index in vector
  (define (read-components file size)
    (unless (<= size 0)
      (let* ((index (read file))
             (component (read file))
             (piko-parts (read file)))
        (vector-set! components index component)
        (vector-set! piko-tracks index piko-parts)
        (read-components file (- size 1)))))
  ;; relaties tussen componenten
  (define (read-relations file)
    (let ((input (read file)))
      (unless (eof-object? input)
      (let ((from (vector-ref components input))
            (to (vector-ref components (read file))))
        (set! connections (cons (cons from to) connections))
        (read-relations file)))))
  (read-components file size)
  (read-relations file)
  (close-input-port file)
  (list components (reverse connections) piko-tracks))


;; ontvangt naam van tekstbestand en opent een filestream ermee
;; + start uitlezen van opstelling
(define (start-parser filename)
  (define in (open-input-file (string-append filename ".txt")))
  ;; aantal componenten inlezen en gebruiken als vector-size
  ;; bevat de componenten in de vorm van symbols
  (define components (make-vector (read in)))
  (read-formation in components))

