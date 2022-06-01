#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                Positional List Without Sentinel                 *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2011  Software Languages Lab                  *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library 
 (positional-list-without-sentinel)
 (export new positional-list? find
         attach-first! attach-last! attach-middle!
         detach-first! detach-last! detach-middle!
         length empty? full? update! peek
         first last has-next? has-previous? next previous)
 (import (except (rnrs base) length list? map for-each)
         ;(a-d positional-list single-linked))
         (a-d positional-list vectorial))
         ;(a-d positional-list double-linked))
         ;(a-d positional-list augmented-double-linked))

 (define (find plst key)
   (define ==? (equality plst))
   (if (empty? plst)
     #f
     (let sequential-search
       ((curr (first plst)))
       (cond
         ((==? key (peek plst curr)) 
          curr)
         ((not (has-next? plst curr))
          #f)
         (else 
          (sequential-search (next plst curr))))))))
