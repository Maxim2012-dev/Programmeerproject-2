#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                            Database                             *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-


(library
 (database)
 (export new open delete!
         create-table drop-table!
         select-from/eq
         alter-table-drop-column!
         insert-into-table! create-index!
         delete-where! print-table print)
 (import (prefix (a-d disk disk) disk:)
         (a-d file constants)
         (srfi :9)
         (prefix (a-d disk file-system) fs:)
         (prefix (a-d db rcid) rcid:)
         (prefix (a-d db table fixed-size-slots table) tbl:)
         (prefix (a-d db table fixed-size-slots schema) scma:)
         (prefix (a-d db index b-tree b-tree) btree:)
         (rnrs base)
         (rnrs control)
         (rnrs lists)
         (only (rnrs io simple) display newline))
 
 (define *num* 0)
 (define (gennum)
   (let ((res *num*))
     (set! *num* (+ *num* 1))
     res))
 
 (define meta-schema:table `((string  ,fs:filename-size)   ; table name
                             (natural 2)))                 ; table id (foreign key to index table)
 (define table:table-name    0)
 (define table:table-id      1)
 
 
 (define meta-schema:indexes `((natural 2)                 ; table identity
                               (string  ,fs:filename-size) ; index name
                               (natural 2)))               ; attribute-number of this index (i.e. in the table)
 (define indexes:tble-idty  0)
 (define indexes:index-name 1)
 (define indexes:key-att    2) 
 
 (define-record-type database
   (make t i)
   database?
   (t tables)
   (i indexes))
 
 (define (new disk name)
   (define tbls (tbl:new disk (string-append "TBL" name) meta-schema:table))
   (define idxs (tbl:new disk (string-append "IDX" name) meta-schema:indexes))
   (make tbls idxs))
 
 (define (open disk name)
   (define tbls (tbl:open disk name))
   (define idxs (tbl:open disk name))
   (make tbls idxs))
 
 (define (create-table dbse name scma)
   (define tbls (tables dbse))
   (define disk (tbl:disk tbls))
   (define tble (tbl:new disk name scma))
   (define idty (gennum))
   (tbl:insert! tbls (list name idty))
   tble)
 
 (define (find-id-in-meta-table dbse tabl)
   (define name (tbl:name tabl))
   (define tbls (tables dbse))
   (tbl:set-current-to-first! (tbl:schema tbls) tbls)
   (let loop
     ((tuple (tbl:peek (tbl:schema tbls) tbls)))       ;; pass the schema of the meta table NEW
     (let ((tble-name (car tuple))
           (tble-idty (cadr tuple)))
       display
       (cond ((string=? tble-name name)
              tble-idty)
             ((not (eq? (tbl:set-current-to-next! (tbl:schema tbls) tbls) no-current))
              (loop (tbl:peek (tbl:schema tbls) tbls)))
             (else
              not-found)))))
 
 (define (for-all-tables dbse proc)
   (define tbls (tables dbse))
   (define disk (tbl:disk tbls))
   (when (not (eq? (tbl:set-current-to-first! (tbl:schema tbls) tbls) no-current))
     (let loop
       ((tuple (tbl:peek (tbl:schema tbls) tbls)))      ;; pass the schema of the meta table NEW
       (let ((tabl (tbl:open disk (list-ref tuple table:table-name))))
         (if (and (proc tabl)
                  (not (eq? (tbl:set-current-to-next! (tbl:schema tbls) tbls) no-current)))
             (loop (tbl:peek (tbl:schema tbls) tbls)))))))
 
 (define (for-all-indices dbse tble proc)
   (define idxs (indexes dbse))
   (define disk (tbl:disk idxs))
   (define idty (find-id-in-meta-table dbse tble))
   (when (not (eq? (tbl:set-current-to-first! (tbl:schema idxs) idxs) no-current))
     (let loop
       ((tuple (tbl:peek (tbl:schema idxs) idxs)))        ;; pass the schema of the indices table NEW
       (cond ((= (list-ref tuple indexes:tble-idty) idty) ; the index belongs to the tble-indx
              (let ((indx (btree:open disk (list-ref tuple indexes:index-name))))
                (if (and (proc indx (list-ref tuple indexes:key-att))
                         (not (eq? (tbl:set-current-to-next! (tbl:schema idxs) idxs) no-current)))
                    (loop (tbl:peek idxs)))))
             ((not (eq? (tbl:set-current-to-next! (tbl:schema idxs) idxs) no-current))
              (loop (tbl:peek (tbl:schema idxs) idxs)))))))
 
 (define (for-all-tuples table proc)
   (if (not (eq? (tbl:set-current-to-first! (tbl:old-scma table) table) no-current))
       (let loop
         ((tuple (tbl:peek (tbl:old-scma table) table)))
         (let ((curr (tbl:current table)))
           (if (and (proc tuple curr)
                    (not (eq? (tbl:set-current-to-next! (tbl:old-scma table) table) no-current)))
               (loop (tbl:peek (tbl:old-scma table) table)))))))

 (define (create-index! dbse tabl name attribute) 
   (define disk (tbl:disk tabl))
   (define tbls (tables dbse))
   (define idxs (indexes dbse))
   (define idty (find-id-in-meta-table dbse tabl))
   (define scma (tbl:schema tabl))
   (define indx (btree:new disk name 
                           (scma:type scma attribute) 
                           (scma:size scma attribute)))
   (tbl:insert! idxs (list idty name attribute)) 
   (for-all-tuples
    tabl
    (lambda (tuple rid)
      (btree:insert! indx (list-ref tuple attribute) rid)))
   (tbl:close! idxs)
   (btree:flush! indx))
 
 (define (insert-into-table! dbse tble tuple)
   (define rcid  (tbl:insert! tble tuple))
   (tbl:close! tble)
   (for-all-indices dbse tble 
                    (lambda (indx att)
                      (btree:insert! indx (list-ref tuple att) rcid)
                      (btree:flush! indx))))
 
 (define (select-from/eq dbse tble attr valu)
   (define scma (tbl:schema tble))
   (define type (scma:type scma attr))
   (define eqls (vector-ref equals type))           ;right equality procedure
   (define indx ())
   (define rslt ())
   (for-all-indices dbse tble (lambda (idx att)     ;first try to find an index on 'attr'
                                (when (= att attr)
                                  (set! indx idx)
                                  #f)))
   (if (null? indx)        ; index on 'attr' found, or search the tuple file sequentially
     (for-all-tuples tble (lambda (tple rcid)
                            (if (eqls (list-ref tple attr) valu)
                              (set! rslt (cons (tbl:peek tble) rslt)))))
     (for-all-identical-keys indx eqls valu
                             (lambda (rcid)
                               (tbl:current! tble (cdr (btree:peek indx)))
                               (set! rslt (cons (tbl:peek tble) rslt)))))
   rslt)

 ;; ==============================================================================================

(define (removeFromList list idx)
  (if (null? list)
      '()
      (if (= idx 0)
          (cdr list)
          (cons (car list) (removeFromList (cdr list) (- idx 1))))))
 
 (define (alter-table-drop-column! database table attribute)
   (define indx ())
   (let* ((dsk (tbl:disk table))
          (atts-list (scma:atts (tbl:schema table)))
          (old-scma (scma:new dsk atts-list)))
     (tbl:old-scma! table old-scma)                               ;; store the original schema for reading tuples
     (scma:remove-attribute! (tbl:schema table) attribute))
   (for-all-tuples table (lambda (tple rcid)
                           (let ((new-tuple (removeFromList tple attribute)))
                             (tbl:replace! table rcid new-tuple))))
   (tbl:old-scma! table (tbl:schema table))                       ;; set the old schema to the new one
   (for-all-indices database table (lambda (idx att)                   ;; try to find an index on 'attribute'
                                     (when (= att attribute)
                                       (set! indx idx)
                                       #f)))
   (when (not (null? indx))                                       ;; index found
     (delete-from-index-table database indx)                      ;; delete index from meta table
     (btree:delete! indx)))                                       ;; delete index from disk
   

 ;; ==============================================================================================

  (define (for-all-identical-keys indx eqls valu proc)
   (let loop
     ((cur? (eq? (btree:find! indx valu) done)))
     (if cur?
       (loop (and (proc (cdr (btree:peek indx)))
                  (eq? (btree:set-current-to-next! indx) done)
                  (eqls (car (btree:peek indx)) valu))))))
 
 (define (delete-from-indexes dbse tble eqls tple rcid)
   (for-all-indices
    dbse tble
    (lambda (indx att)
      (for-all-identical-keys indx eqls (list-ref tple att)
                              (lambda (rcid2) ; but only if it is THIS tuple?
                                (when (equal? rcid2 rcid)
                                  (btree:delete! indx)
                                  #f)))
      (btree:flush! indx))))

 (define (find-tuple-rcid dbse tble eqls attr valu)
   (define indx ())
   (define rcid ())
   (for-all-indices dbse tble (lambda (idx att) ;first try to find an index on 'attr'
                                (when (= att attr)
                                  (set! indx idx)
                                  #f)))
   (cond ((not (null? indx)) ; exists index leading to the tuple
          (when (eq? (btree:find! indx valu) done)
            (set! rcid (cdr (btree:peek indx)))
            (tbl:current! tble rcid)))
         (else               ; there is no index => search tuple sequentially in the table
          (for-all-tuples tble (lambda (tple rid2)
                                 (when (eqls (list-ref tple attr) valu)
                                   (set! rcid rid2)
                                   #f)))))
   rcid)
  
 (define (delete-where! dbse tble attr valu)
   (define scma (tbl:schema tble))
   (define type (scma:type scma attr))
   (define eqls (vector-ref equals type))
   (let loop
     ((rcid (find-tuple-rcid dbse tble eqls attr valu)))
     (unless (null? rcid)
       (let ((tple (tbl:peek tble)))
         (tbl:delete!  tble rcid)
         (tbl:close! tble)
         (delete-from-indexes dbse tble eqls tple rcid))
       (loop (find-tuple-rcid dbse tble eqls attr valu)))))

;; ==============================================================================================

 ;; delete an index from the meta table of indexes
 (define (delete-from-index-table dbse indx)
   (define name  (btree:name indx))
   (define indxs (indexes dbse))
   (tbl:set-current-to-first! (tbl:schema indxs) indxs)
   (let find-index
     ((tuple (tbl:peek (tbl:schema indxs) indxs)))
     (let ((tble-name (cadr tuple)))
       (cond ((string=? tble-name name)
              (tbl:delete! indxs (tbl:current indxs)))
             ((not (eq? (tbl:set-current-to-next! (tbl:schema indxs) indxs) no-current))
              (find-index (tbl:peek (tbl:schema indxs) indxs)))
             (else not-found)))))

;; ==============================================================================================
 
 (define (delete-from-meta-table dbse tabl)
   (define name (tbl:name tabl))
   (define tbls (tables dbse))
   (tbl:set-current-to-first! (tbl:schema tbls) tbls)
   (let find-table
     ((tuple (tbl:peek tbls)))
     (let ((tble-name (car tuple)))
       (cond ((string=? tble-name name)
              (tbl:delete! tbls (tbl:current tbls)))
             ((not (eq? (tbl:set-current-to-next! (tbl:schema tbls) tbls) no-current))
              (find-table (tbl:peek tbls)))
             (else not-found)))))
 
 (define (drop-table! dbse table)
   (define tbls (tables dbse))
   (define idxs (indexes dbse))
   (define disk (tbl:disk tbls))
   (for-all-indices dbse table 
                    (lambda (indx att)
                      (btree:drop! indx)))
   (delete-from-meta-table dbse table)
   (tbl:drop! table))
 
 (define (delete! dbse)
   (define tbls (tables dbse))
   (define idxs (indexes dbse))
   (for-all-tables dbse
                   (lambda (table)
                     (drop-table! dbse table)))
   (tbl:drop! tbls)
   (tbl:drop! idxs))
 
 (define (print dbse)
   (define tbls (tables dbse))
   (define idxs (indexes dbse))
   (display "DATABASE   METATABLE of TABLES")(newline)
   (display "           ===================")(newline)
   (tbl:print tbls)
   (display "           METATABLE of INDEXES")(newline)
   (display "           ===================")(newline)
   (tbl:print idxs))
 (define (print-table db tble)
   (tbl:print tble)))