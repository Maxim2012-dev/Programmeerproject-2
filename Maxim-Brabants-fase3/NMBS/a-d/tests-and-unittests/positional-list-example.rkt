#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                     Positional List Example                     *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import
 (prefix (a-d positional-list adt) plist:)
 (except (rnrs base) length)
 (srfi :9)
 (rnrs io simple))

(define-record-type event
  (make-event d m n)
  event?
  (d day)
  (m month)
  (n note))

(define event-eq? 
  (lambda (event1 event2)
    (and (eq? (day event1) (day event2))
         (eq? (month event1) (month event2)))))
(define todo-list (plist:new event-eq?))

(define todo-list-event-1 (make-event 5 10 "Give Lecture on Strings"))
(define todo-list-event-2 (make-event 12 10 "Give Lecture on Linearity"))
(define todo-list-event-3 (make-event 19 10 "Give Lecture Sorting"))
(plist:add-after! todo-list todo-list-event-1)
(plist:add-after! todo-list todo-list-event-2)
(plist:add-after! todo-list todo-list-event-3)
(define lecture-2 (plist:find todo-list (make-event 12 10 '())))
(plist:add-before! todo-list (make-event 8 10 "Prepare Lecture on Linearity") lecture-2)
(define prepare-lecture (plist:find todo-list (make-event 8 10 '())))
(plist:add-after! todo-list (make-event 9 10 "Have a Rest") prepare-lecture)
(plist:for-each todo-list (lambda (event)
                            (display (list "On " (day event) "/" (month event) ": " (note event)))
                            (newline)))

;; Experiment to illustrate the problem with positional lists

(define lectures (list (plist:find todo-list (make-event 5 10 '()))
                       (plist:find todo-list (make-event 12 10 '()))
                       (plist:find todo-list (make-event 19 10 '()))))

(define rest (plist:find todo-list (make-event 9 10 '())))
(plist:add-after! todo-list (make-event 11 10 "Go out with friends") rest)
(map (lambda (pos)
       (display (note (plist:peek todo-list pos)))
       (newline))
     lectures)
