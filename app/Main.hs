{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# Language GADTs #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}

module Main where

import Debug.Trace (trace)

import BasicPrelude
import CorePrelude
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Data.Semigroup
import Data.Maybe (fromJust)

import qualified Graph as G
import qualified UGraph as UG
import qualified DGraph as DG
import qualified WGraph as WG
import qualified DWGraph as DW
import qualified UWGraph as UW
import Distro
import AdjacenceMatrix
import GraphAlgorithms
import PriorityQueue

main :: IO ()
main = someFunc

--ugraph = G.emptyGraph::(UnweightedAdjGraph Int)
ugraph = UG.add_edge 0 1
       . UG.add_edge 1 2
       . UG.add_edge 2 7
       . UG.add_edge 2 4
       . UG.add_edge 2 3
       . UG.add_edge 1 5
       . UG.add_edge 5 6
       . UG.add_edge 3 6
       . UG.add_edge 3 4
       . UG.add_edge 6 8
       $ G.emptyGraph::(UnweightedAdjGraph Int)
dgraph =  DG.add_edge 0 1
        . DG.add_edge 1 2
        . DG.add_edge 2 7
        . DG.add_edge 2 4
        . DG.add_edge 2 3
        . DG.add_edge 1 5
        . DG.add_edge 5 6
        . DG.add_edge 3 6
        . DG.add_edge 3 4
        . DG.add_edge 6 8
        $ G.emptyGraph::(UnweightedAdjGraph Int)
ugraph1 = UG.add_edge 0 1
        . UG.add_edge 1 2
        . UG.add_edge 1 3
        . UG.add_edge 2 3
        . UG.add_edge 1 4
        . UG.add_edge 3 5
        . UG.add_edge 5 4
        . UG.add_edge 3 6
        . UG.add_edge 6 7
        . UG.add_edge 0 7
        $ G.emptyGraph::(UnweightedAdjGraph Int)
dgraph1 = DG.add_edge 0 1
        . DG.add_edge 1 2
        . DG.add_edge 1 3
        . DG.add_edge 2 3
        . DG.add_edge 1 4
        . DG.add_edge 3 5
        . DG.add_edge 5 4
        . DG.add_edge 3 6
        . DG.add_edge 6 7
        . DG.add_edge 0 7
        $ G.emptyGraph::(UnweightedAdjGraph Int)
dwgraph1 = DW.add_edge 'a' 'b' 2
         . DW.add_edge 'a' 'c' 3
         . DW.add_edge 'b' 'd' 2
         . DW.add_edge 'c' 'e' 6
         . DW.add_edge 'e' 'b' 5
         . DW.add_edge 'e' 'd' 4
         $ G.emptyGraph::(WeightedAdjGraph Char Int)
uwgraph1 = UW.add_edge 'a' 'b' 2
         . UW.add_edge 'a' 'c' 3
         . UW.add_edge 'b' 'd' 2
         . UW.add_edge 'c' 'e' 6
         . UW.add_edge 'e' 'b' 5
         . UW.add_edge 'e' 'd' 4
         $ G.emptyGraph::(WeightedAdjGraph Char Int)
uwgraph2 = UW.add_edge 0 1 1
         . UW.add_edge 1 2 2
         . UW.add_edge 1 3 2
         . UW.add_edge 2 3 2
         . UW.add_edge 1 4 3
         . UW.add_edge 3 5 1
         . UW.add_edge 5 4 3
         . UW.add_edge 3 6 1
         . UW.add_edge 6 7 1
         . UW.add_edge 7 0 1
         $ G.emptyGraph::(WeightedAdjGraph Int Int)

someFunc :: IO ()
someFunc = do
    putStrLn "-START---------------------------"
--    putStrLn $ tshow $ ugraph 
    putStrLn "-bread first search--------------"
    putStrLn $ tshow $ breadth_first ugraph 2 6
    putStrLn $ tshow $ breadth_first ugraph 2 9
    putStrLn "-getting indegrees---------------"
    putStrLn $ tshow $ (DG.get_indegrees dgraph::(PMF Int Int))
    putStrLn "-topological sort----------------"
    putStrLn $ tshow $ topological_sort dgraph
    putStrLn "-shortest path undirected unweighted-"
    putStrLn $ tshow $ ((shortest_path_unweighted ugraph1 0 5)::[Int])
    putStrLn $ tshow $ ((shortest_path_unweighted ugraph1 0 6)::[Int])
    putStrLn $ tshow $ ((shortest_path_unweighted ugraph1 7 4)::[Int])
    putStrLn "-shortest path directed unweighted-"
    putStrLn $ tshow $ ((shortest_path_unweighted dgraph1 0 5)::[Int])
    putStrLn $ tshow $ ((shortest_path_unweighted dgraph1 0 6)::[Int])
    putStrLn "-dijkstra shortest path weighted-"
    putStrLn $ tshow $ ((dijkstra (emptyPriorityQueue::TrivialPQ Char (Infinite Int) ) dwgraph1 'a' 'd')::(Infinite Int, [Char]))
    putStrLn "-prims alg spanning tree---------"
    let priorityQueueConstructor1 = (emptyPriorityQueue::(TrivialPQ (Char,Char) Int))
    let priorityQueueConstructor2 = (emptyPriorityQueue::(TrivialPQ (Int,Int) Int))
    putStrLn $ tshow $ ((prims priorityQueueConstructor1 uwgraph1 'e')::(WeightedAdjGraph Char Int))
    putStrLn $ tshow $ ((prims priorityQueueConstructor2 uwgraph2 1)::(WeightedAdjGraph Int Int))
    putStrLn "---------------------------------"
    putStrLn "-END-----------------------------"
