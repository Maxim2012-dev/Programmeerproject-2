#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*           Undirected unweighted Graph Algorithms Tests          *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (rnrs base)
        (rnrs io simple)
        (a-d graph-algorithms undirected basic)
        (a-d graph-algorithms undirected connectivity)
        (a-d graph-algorithms undirected dft-applications)
        (a-d graph-algorithms undirected bft-applications)
        (a-d graph-traversing dft-characterisations)
        (a-d graph-traversing bft-characterisations)
        (a-d graph examples undirected-unweighted))

(display "TEST BASIC ALGORITHMS")(newline)
(display "---------------------")(newline)

(display (list "degrees connected =" (degrees connected)))(newline)
(display (list "degrees three-cc =" (degrees three-cc)))(newline)
(display (list "degrees kite =" (degrees kite)))(newline)
(display (list "degrees cyc-yes =" (degrees cyc-yes)))(newline)
(display (list "degrees cyc-no =" (degrees cyc-no)))(newline)
(display (list "connected? 2 to 5 in connected" (connected? connected 2 5)))(newline)
(display (list "connected? 2 to 5 in three-cc" (connected? three-cc 2 5)))(newline)
(display (list "connected? 2 to 5 in kite" (connected? kite 2 5)))(newline)
(display (list "connected? 2 to 5 in cyc-yes" (connected? cyc-yes 2 5)))(newline)
(display (list "connected? 2 to 5 in cyc-no" (connected? cyc-no 2 5)))(newline)

(display "START DFS-BFS APPLICATIONS TEST")(newline)
(display "-------------------------------")(newline)
(display (list "cyc-no = " (cyclic? cyc-no)))(newline)
(display (list "cyc-yes = " (cyclic? cyc-yes)))(newline)
(display (list "cyc-no = " (cyclic? cyc-no)))(newline)
(display (list "cyc-yes = " (cyclic? cyc-yes)))(newline)
(display (list "2-5 connected in connected" (exists-path? connected 1 5)))
(display (list "2-5 connected in 3cc" (exists-path? three-cc 1 5)))
(display (list "2-5 connected in kite" (exists-path? kite 1 5)))
(display (list "shortest path in cyc-yes from" 0 " to " 1 " is " (shortest-path cyc-yes 0 1)))(newline)
(display (list "shortest path in cyc-no from" 0 " to " 1 " is " (shortest-path cyc-no 0 1)))(newline)
(display (list "The distance between 0 and 1 in cyc-yes is " (distance cyc-yes 0 1)))
(display (list "The distance between 0 and 1 in cyc-no is " (distance cyc-no 0 1)))

(display "START CONNECTIVITY TEST")(newline)
(display "-----------------------")(newline)
(display (list "2-5 connected in connected" (connected-components/dft connected)))(newline)
(display (list "2-5 connected in 3cc" (connected-components/dft three-cc)))(newline)
(display (list "2-5 connected in kite" (connected-components/dft kite)))(newline)
(display (list "2-5 connected in connected" (connected-components/bft connected)))(newline)
(display (list "2-5 connected in 3cc" (connected-components/bft three-cc)))(newline)
(display (list "2-5 connected in kite" (connected-components/bft kite)))(newline)
(display (list "biconnected components connected= " (biconnected-components connected)))(newline)
(display (list "biconnected components 3cc= " (biconnected-components three-cc)))(newline)
(display (list "biconnected components kite= " (biconnected-components kite)))(newline)
(display (list "edge-connected-components connected= " (edge-connected-components connected)))(newline)
(display (list "edge-connected-components 3cc= " (edge-connected-components three-cc)))(newline)
(display (list "edge-connected-components kite= " (edge-connected-components kite)))(newline)
(display (list "graph undir-a: [ connected components = " (connected-components/dft undir-a) "] ; [ biconnected-components = " (biconnected-components  undir-a) "] ; [ edge-connected-components = " (edge-connected-components undir-a) "]"))(newline)
(display (list "graph undir-a: [ connected components = " (connected-components/bft undir-a) "] ; [ biconnected-components = " (biconnected-components  undir-a) "] ; [ edge-connected-components = " (edge-connected-components undir-a) "]"))(newline)
(display (list "graph undir-b: [ connected components = " (connected-components/dft undir-b) "] ; [ biconnected-components = " (biconnected-components  undir-b) "] ; [ edge-connected-components = " (edge-connected-components undir-b) "]"))(newline)
(display (list "graph undir-b: [ connected components = " (connected-components/bft undir-b) "] ; [ biconnected-components = " (biconnected-components  undir-b) "] ; [ edge-connected-components = " (edge-connected-components undir-b) "]"))(newline)
(display (list "graph undir-c: [ connected components = " (connected-components/dft undir-c) "] ; [ biconnected-components = " (biconnected-components  undir-c) "] ; [ edge-connected-components = " (edge-connected-components undir-c) "]"))(newline)
(display (list "graph undir-c: [ connected components = " (connected-components/bft undir-c) "] ; [ biconnected-components = " (biconnected-components  undir-c) "] ; [ edge-connected-components = " (edge-connected-components undir-c) "]"))(newline)
(display (list "bipartite/dfs for connected are " (bipartite/dft? connected)))(newline)
(display (list "bipartite/bfs for connected are " (bipartite/bft? connected)))(newline)
(display (list "bipartite/dfs for 3cc are " (bipartite/dft? three-cc)))(newline)
(display (list "bipartite/bfs for 3cc are " (bipartite/bft? three-cc)))(newline)
(display (list "bipartite/dfs for kite are " (bipartite/dft? kite)))(newline)
(display (list "bipartite/bfs for kite are " (bipartite/bft? kite)))(newline)
(display (list "bipartite/dfs for bipart are " (bipartite/dft? bipart)))(newline)
(display (list "bipartite/bfs for bipart are " (bipartite/bft? bipart)))(newline)


(display "START DFS-TEST")(newline)
(display "--------------")(newline)
(display (list "a tree for 3cc ==>" (dft-forest three-cc)))(newline)
(display (list "a tree for connected ==>" (dft-forest connected)))(newline)
(display (list "a tree for kite ==>" (dft-forest kite)))(newline)
(display (list "node numbers for connected ==>" (dft-node-numbering connected)))(newline)
(display (list "node numbers for 3cc ==>" (dft-node-numbering three-cc)))(newline)
(display (list "node numbers for kite ==>" (dft-node-numbering kite)))(newline)
(display (list "edge classification for connected ==>" (dft-undirected-edge-classification connected)))(newline)
(display (list "edge classification for 3cc ==>" (dft-undirected-edge-classification three-cc)))(newline)
(display (list "edge classification for kite ==>" (dft-undirected-edge-classification kite)))(newline)

(display "START BFS-TEST")(newline)
(display "--------------")(newline)
(display (list "a tree for 3cc ==>" (bft-forest connected)))(newline)
(display (list "a tree for connected ==>" (bft-forest connected)))(newline)
(display (list "a tree for kite ==>" (bft-forest kite)))(newline)
(display (list "node numbers for connected ==>" (bft-node-numbering connected)))(newline)
(display (list "node numbers for 3cc ==>" (bft-node-numbering three-cc)))(newline)
(display (list "node numbers for kite ==>" (bft-node-numbering kite)))(newline)
(display (list "edge classification for connected ==>" (bft-undirected-edge-classification connected)))(newline)
(display (list "edge classification for 3cc ==>" (bft-undirected-edge-classification three-cc)))(newline)
(display (list "edge classification for kite ==>" (bft-undirected-edge-classification kite)))(newline)

