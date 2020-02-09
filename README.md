# GraphTheory
implementing some graph theoretical algorithms in haskell.

see: https://en.wikipedia.org/wiki/Graph_theory

only for joy. making production ready is not intended. memory consumption, performance etc. issues may occur.

##DONE
+ base class
+ simple adjacent matrix implementation using HashMap
+ beadth first
+ topological sort 
+ shortest parh
+ dijkstra

##WORKING
+ refactor Graph class - dont like the way priority queue is instantiated
+ refactor Graph class - intorduce special Weighted Graph strucutre
+ kruskal

##TODO
+ dijkstra - why doesn't terminate when has cycles all edges have same weight??? (unweighted graph)
+ dijkstra - optimize alg when there is no way, currently runtime error 
+ dijkstra - provide a faster implementation for priority queue
+ prim
+ floyd warshall
+ hungarian
+ try some real world problems 
+ replace lists by array or vector
+ clean up main and lib


