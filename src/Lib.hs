{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# Language GADTs #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}

module Lib
    where

import Debug.Trace (trace)

import BasicPrelude
import CorePrelude
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Data.Semigroup
import Data.Maybe (fromJust)

import Graph
import AdjacenceMatrix
import GraphAlgorithms
import PriorityQueue

ugraph = add_edge_undir 0 1
       . add_edge_undir 1 2
       . add_edge_undir 2 7
       . add_edge_undir 2 4
       . add_edge_undir 2 3
       . add_edge_undir 1 5
       . add_edge_undir 5 6
       . add_edge_undir 3 6
       . add_edge_undir 3 4
       . add_edge_undir 6 8
       $ emptyGraph::(UnweightedAdjGraph Int)
dgraph =  add_edge 0 1
        . add_edge 1 2
        . add_edge 2 7
        . add_edge 2 4
        . add_edge 2 3
        . add_edge 1 5
        . add_edge 5 6
        . add_edge 3 6
        . add_edge 3 4
        . add_edge 6 8
        $ emptyGraph::(UnweightedAdjGraph Int)
ugraph1 = add_edge_weighted_undir 0 1 (1::Int)
        . add_edge_weighted_undir 1 2 (1::Int)
        . add_edge_weighted_undir 1 3 (1::Int)
        . add_edge_weighted_undir 2 3 (1::Int)
        . add_edge_weighted_undir 1 4 (1::Int)
        . add_edge_weighted_undir 3 5 (1::Int)
        . add_edge_weighted_undir 5 4 (1::Int)
        . add_edge_weighted_undir 3 6 (1::Int)
        . add_edge_weighted_undir 6 7 (1::Int)
        . add_edge_weighted_undir 0 7 (1::Int)
        $ emptyGraph::(WeightedAdjGraph Int Int)
dgraph1 = add_edge_weighted 0 1 (1::Int)
        . add_edge_weighted 1 2 (1::Int)
        . add_edge_weighted 1 3 (1::Int)
        . add_edge_weighted 2 3 (1::Int)
        . add_edge_weighted 1 4 (1::Int)
        . add_edge_weighted 3 5 (1::Int)
        . add_edge_weighted 5 4 (1::Int)
        . add_edge_weighted 3 6 (1::Int)
        . add_edge_weighted 6 7 (1::Int)
        . add_edge_weighted 0 7 (1::Int)
        $ emptyGraph::(WeightedAdjGraph Int Int)
dwgraph1 = add_edge_weighted 'a' 'b' (2::Int)
         . add_edge_weighted 'a' 'c' (3::Int)
         . add_edge_weighted 'b' 'd' (2::Int)
         . add_edge_weighted 'c' 'e' (6::Int)
         . add_edge_weighted 'e' 'b' (5::Int)
         . add_edge_weighted 'e' 'd' (4::Int) -- <-- typecast looks ugly
         $ emptyGraph::(WeightedAdjGraph Char Int)
--dwgraph1 = add_edge_weighted 'a' 'b' 2
--         . add_edge_weighted 'a' 'c' 3
--         . add_edge_weighted 'b' 'd' 2
--         . add_edge_weighted 'c' 'e' 6
--         . add_edge_weighted 'e' 'b' 5
--         . add_edge_weighted 'e' 'd' 4
--         $ emptyGraph::(WeightedAdjGraph Char Int)

someFunc :: IO ()
someFunc = do
    putStrLn "HALLO"
    putStrLn $ tshow $ dgraph 
    putStrLn $ tshow $ breadth_first ugraph 2
    putStrLn $ tshow $ (get_indegrees dgraph::(PMF Int Int))
    putStrLn $ tshow $ topological_sort dgraph
    putStrLn "---------------------------------"
    putStrLn $ tshow $ ((shortest_path_unweighted ugraph1 0 5)::[Int])
    putStrLn $ tshow $ ((shortest_path_unweighted ugraph1 0 6)::[Int])
    putStrLn $ tshow $ ((shortest_path_unweighted ugraph1 7 4)::[Int])
    putStrLn "---------------------------------"
    putStrLn $ tshow $ ((shortest_path_unweighted dgraph1 0 5)::[Int])
    putStrLn $ tshow $ ((shortest_path_unweighted dgraph1 0 6)::[Int])
    putStrLn "---------------------------------"
    putStrLn $ tshow $ ((dijkstra (emptyPriorityQueue::TrivialPQ Char (Infinite Int) ) dwgraph1 'a' 'd')::(Infinite Int, [Char]))
