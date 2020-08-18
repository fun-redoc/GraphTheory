# GraphTheory
implementing some graph theoretical algorithms in haskell.

see: https://en.wikipedia.org/wiki/Graph_theory

only for joy. making production ready is not intended. memory consumption, performance etc. issues may occur.

##WORKING
+ there is need for a separate class for undirected graphs and the algs only valid for undirected graphs must be refactored => the class hierarchie must be rebuild

##DONE
+ fix bug in prims algorithm (currently finds the longest spanning tree), priority queue implementation should allow parametrization for the order
+ kruskal
+ base class
+ simple adjacent matrix implementation using HashMap
+ beadth first
+ topological sort 
+ shortest parh
+ dijkstra
+ refactor Graph class - dont like the way priority queue is instantiated
+ refactor Graph class - intorduce special Weighted Graph strucutre
+ prim

##TODO
+ move build settings from cabal file to package.yaml
+ dijkstra - why doesn't terminate when has cycles all edges have same weight??? (unweighted graph)
+ dijkstra - optimize alg when there is no way, currently runtime error 
+ dijkstra - provide a faster implementation for priority queue
+ introduce consistent error handling instead of error statement
+ floyd warshall
+ hungarian
+ try some real world problems 
+ replace lists by array or vector
+ clean up main and lib


