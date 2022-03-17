#lang racket

(define in (open-input-file "loop-switches.txt"))

;; aantal componenten inlezen en gebruiken als vector-size
(define components (make-vector (string->number (read-line in 'any))))

(define (read-formation file)
  (define size (string->number (read-line file 'any)))
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
        (read-relations file))))
  (read-components file size)
  (read-relations file))

(call-with-input-file "loop-switches.txt" read-formation)
(close-input-port in)