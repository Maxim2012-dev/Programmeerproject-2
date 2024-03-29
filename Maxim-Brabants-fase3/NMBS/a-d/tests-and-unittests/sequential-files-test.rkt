#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                      Sequential Files Test                      *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (prefix (a-d disk config) disk:)
        (prefix (a-d disk file-system) fs:)
        (prefix (a-d file sequential output-file) out:)
        (prefix (a-d file sequential input-file) in:)
        (rnrs base)
        (rnrs control)
        (rnrs io simple)
        (a-d scheme-tools))
 
(define d (disk:new "testdisk"))
(fs:format! d)
(newline)(display (list "free=" (fs:df d)))(newline)
(define f (out:new d "testnum")) ; 5 bytes per number
(fs:ls d)
(out:close-write! f)
(newline)(display (list "free=" (fs:df d)))(newline)
(display "--------------------------START WRITING--------------------------")(newline)
(set! f (out:open-write! d "testnum"))(display "begin")
(newline)
(fs:ls d)(newline)
(let loop 
  ((nr 0))
  (out:write! f nr)
  (display (list "just wrote " nr))
  (when (< nr 20)
    (loop (+ nr 1))))
(display "END WRITING")(newline)
(newline)(display (fs:ls d))(newline)
(out:close-write! f)
(newline)(display (fs:ls d))(newline)
(newline)(display (list "free=" (fs:df d)))(newline)
(display "dir:")(newline)
(fs:ls d)

(display ">>>>----START READING")(newline)
(set! f (in:open-read! d "testnum"))
(let loop 
  ((nr 0))
  (when (<= nr 20)
    (display (list "just read "(in:read f)))
    (loop (+ nr 1))))
(display "END READING")(newline)
(in:close-read! f)
(display "--------------------------START WRITING AGAIN--------------------------")(newline)
(set! f (out:open-write! d "testnum"))
(let loop 
  ((nr 0))
  (out:write! f (* 10 nr))
  (display (list "just wrote " (* 10 nr)))
  (when (< nr 50)
    (loop (+ nr 1))))
(display "END WRITING AGAIN")(newline)
(out:close-write! f)

(display ">>>>----START READING AGAIN")(newline)
(set! f (in:open-read! d "testnum"))
(let loop 
  ((nr 0))
  (when (< nr 60)
    (display (list "has more?" (in:has-more? f)))
    (when (in:has-more? f)
      (display (list "just peeked" (in:peek f)))
      (display (list "just read "(in:read f)))
      (loop (+ nr 1)))))
(display "END READING")(newline)
(in:close-read! f)
(newline)
(newline)
(display "--------------------------START WRITING REALS--------------------------")(newline)
(set! f (out:open-write! d "testnum"))
(let loop 
  ((nr 0))
  (out:write! f  (/ nr 3))
  (display (list "just wrote " (/ nr 3)))
  (when (< nr 70)
    (loop (+ nr 1))))
(display "END WRITING AGAIN")(newline)
(out:close-write! f)

(display ">>>>----START READING")(newline)
(set! f (in:open-read! d "testnum"))
(let loop 
  ((nr 0))
  (display (list "just read "(in:read f)))
  (when (< nr 70)
    (loop (+ nr 1))))
(display "END READING")(newline)
(in:close-read! f)

(display "--------------------------START WRITING STRINGS")(newline)
(set! f (out:open-write! d "testnum"))
(out:write! f  "hallo, dit is een leuke test waarbij") ; 36
(out:write! f  " we proberen van een") ; 20
(out:write! f  " aantal strings in een sequentiële file te gieten. Dat doen we door") ; 67
(out:write! f  " de seq:write functie voldoende vaak")
(out:write! f  " op te roepen.")
(display "END WRITING AGAIN")(newline)
(out:close-write! f)

(display ">>>>----START READING")(newline)
(set! f (in:open-read! d "testnum"))
(let loop 
  ((nr 0))
  (if (in:has-more? f)
      (display (list "just read "(in:read f)))(newline))
  (when (< nr 10)
    (loop (+ nr 1))))
(display "END READING")(newline)
(in:close-read! f)

(newline)
(display "THE ULTIMATE TEST")
(define (gen-nat)
  (random-inbetween 1 100))
(define (gen-rea)
  (inexact (/ (random-inbetween 1 100)
              (random-inbetween 1 100))))
(define (gen-str)
  (define len (random-inbetween 1 200))
  (do ((idx 0 (+ idx 1))
       (res "" (string-append res (string (integer->char (random-inbetween 48 100))))))
    ((= idx len)
     res)))
(define (gen-int)
  (- (random-inbetween 1 100)))
(define (disp)
  (let ((r (random-inbetween 1 4)))
    (case r
      ((1) gen-nat)
      ((2) gen-rea)
      ((3) gen-str)
      ((4) gen-int))))

(define vec (make-vector 100))
(set! f (out:open-write! d "testnum"))
(do ((idx 0 (+ idx 1)))
  ((= idx 100))
  (let* ((g (disp))
         (v (g)))
    (vector-set! vec idx v)
    (out:write! f v)))
(out:close-write! f)

(newline)
(display "DISK :")(newline)

(display ">>>>----START READING")(newline)
(set! f (in:open-read! d "testnum"))
(do ((idx 0 (+ idx 1)))
  ((= idx 100))
  (let* ((v (in:read f)))
    (display (if (not ((if (number? v)
                           =
                           string=?)
                       v
                       (vector-ref vec idx)))
                 (display (list "ERROR: unequal" v (vector-ref vec idx)))
                 'ok))))
(display "END READING")(newline)
(in:close-read! f)