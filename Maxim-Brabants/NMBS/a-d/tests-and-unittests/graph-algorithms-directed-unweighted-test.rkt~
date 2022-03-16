#!r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*             Directed unweighted Graph Algorithms Tests          *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (rnrs base)
        (rnrs io simple)
       ;(a-d graph-algorithms directed config)
        (a-d graph-algorithms directed basic)
        (a-d graph-algorithms directed connectivity)
        (a-d graph-algorithms directed topological-sorting)
        (a-d graph-algorithms directed traclo-unweighted)
        (a-d graph-algorithms directed single-source-shortest-path)
        (a-d graph examples directed-unweighted))

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                              Tests                              *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(newline)
(display (list "in-deg/out-deg=" (in/out-degrees dag-1)))(newline)

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                              Tests                              *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
(newline)(display "            INPUT =")(display  sedgewick172)
(newline)(display "TRACLO VERY NAIVE =")(display (traclo-very-naive (copy sedgewick172)))
(newline)(display "TRACLO      NAIVE =")(display (traclo-naive (copy sedgewick172)))
(newline)(display "TRACLO   WARSHALL =")(display (traclo-warshall (copy sedgewick172)))
(newline)(display "TRACLO  WARSHALL* =")(display (traclo-warshall* (copy sedgewick172)))
(newline)(display "TRACLO        DFS =")(display (traclo-dfs (copy sedgewick172)))
(newline)(display "            INPUT =")(display  full-cycle)
(newline)(display "TRACLO VERY NAIVE =")(display (traclo-very-naive full-cycle))
(newline)(display "TRACLO      NAIVE =")(display (traclo-naive full-cycle))
(newline)(display "TRACLO   WARSHALL =")(display (traclo-warshall (copy full-cycle)))
(newline)(display "TRACLO  WARSHALL* =")(display (traclo-warshall* (copy full-cycle)))
(newline)(display "TRACLO        DFS =")(display (traclo-dfs (copy full-cycle)))
(display (list "SCC-kosa sedgewick172" (scc-kosaraju sedgewick172)))(newline)
(display (list "SCC-tarj sedgewick172" (scc-tarjan sedgewick172)))(newline)
(display (list "SCC-kosa full-cycle" (scc-kosaraju full-cycle)))(newline)
(display (list "SCC-tarj full-cycle" (scc-tarjan full-cycle)))(newline)
(display (list "SCC-kosa acyc" (scc-kosaraju a-list)))(newline)
(display (list "SCC-tarj acyc" (scc-tarjan a-list)))(newline)
(display (list "SCC-kosa scc4" (scc-kosaraju scc4)))(newline)
(display (list "SCC-tarj scc4" (scc-tarjan scc4)))(newline)
;(display (list "SCC-gabow" (scc-gabow sedgewick172)))(newline)
;(display (list "SCC-gabow" (scc-gabow full-cycle)))(newline)
;(display (list "SCC-gabow" (scc-gabow acyc)))(newline)
;(display (list "SCC-gabow" (scc-gabow scc4)))(newline)
;(display (list "SCC-traclo" (scc-traclo scc4)))(newline)

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                              Tests                              *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(newline)
(display (dfs-topological-sort dag-1))(newline)
(display (bfs-topological-sort dag-1))(newline)

