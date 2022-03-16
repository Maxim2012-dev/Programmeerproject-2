#!r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*           Undirected weighted Graph Algorithms Tests            *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (rnrs base)
        (rnrs io simple)
        (a-d graph-algorithms undirected mst)
        (a-d graph examples undirected-weighted))

(display "START TEST")(newline)
(display "----------")(newline)

(display (mst-kruskal city-plan))(newline)
(display (mst-prim-jarnik city-plan))(newline)
(display (mst-boruvka city-plan))(newline)
(display (mst-kruskal cormen571))(newline)
(display (mst-prim-jarnik cormen571))(newline)
(display (mst-boruvka cormen571))(newline)
(display (mst-kruskal triangle))(newline)
(display (mst-prim-jarnik triangle))(newline)
(display (mst-boruvka triangle))(newline)