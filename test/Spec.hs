{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# Language GADTs #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
module Main where
import BasicPrelude
import CorePrelude
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Data.Semigroup
import Data.Maybe (fromJust)
import Test.QuickCheck
import Test.HUnit

import Graph
import AdjacenceMatrix
import GraphAlgorithms
import PriorityQueue

uwgraph1 = add_edge_weighted_undir 'a' 'b' 2
         . add_edge_weighted_undir 'a' 'c' 3
         . add_edge_weighted_undir 'b' 'd' 2
         . add_edge_weighted_undir 'c' 'e' 6
         . add_edge_weighted_undir 'e' 'b' 5
         . add_edge_weighted_undir 'e' 'd' 4
         $ emptyGraph::(WeightedAdjGraph Char Int)
uwgraph2 = add_edge_weighted_undir 0 1 1
         . add_edge_weighted_undir 1 2 2
         . add_edge_weighted_undir 1 3 2
         . add_edge_weighted_undir 2 3 2
         . add_edge_weighted_undir 1 4 3
         . add_edge_weighted_undir 3 5 1
         . add_edge_weighted_undir 5 4 3
         . add_edge_weighted_undir 3 6 1
         . add_edge_weighted_undir 6 7 1
         . add_edge_weighted_undir 7 0 1
         $ emptyGraph::(WeightedAdjGraph Int Int)

test_prims_1 = 
  TestCase ( do let priorityQueueConstructor = (emptyPriorityQueue::(TrivialPQ (Char,Char) Int))
                let tree = ((prims priorityQueueConstructor uwgraph1 'e')::(WeightedAdjGraph Char Int)) 
                assertEqual "spanning tree and original graph must have same number of verticies" 
                            (num_vertices uwgraph1) (num_vertices tree)
                assertEqual "spanning tree has no cycles"
                            (isJust $topological_sort tree) True
                putStrLn "\n"
--                putStrLn $ tshow $ tree
           )
test_prims_2 = 
  TestCase ( do let priorityQueueConstructor = (emptyPriorityQueue::(TrivialPQ (Int,Int) Int))
                let tree = ((prims priorityQueueConstructor uwgraph2 1)::(WeightedAdjGraph Int Int)) 
                assertEqual "spanning tree and original graph must have same number of verticies"
                            (num_vertices uwgraph2) (num_vertices tree)
                assertEqual "spanning tree has no cycles"
                            (isJust $topological_sort tree) True
                putStrLn "\n"
--                putStrLn $ tshow $ tree
           )
test_kruskal_1 = 
  TestCase ( do let priorityQueueConstructor = (emptyPriorityQueue::(TrivialPQ (Char,Char) Int))
                let tree = ((kruskal priorityQueueConstructor uwgraph1)::(WeightedAdjGraph Char Int)) 
                assertEqual "spanning tree and original graph must have same number of verticies" 
                            (num_vertices uwgraph1) (num_vertices tree)
                assertEqual "spanning tree has no cycles"
                            (isJust $topological_sort tree) True
                putStrLn "\n"
                putStrLn $ tshow $ tree
           )

tests = TestList [ TestLabel "prims alg test 1" test_prims_1
                 , TestLabel "prims alg test 2" test_prims_2 
                 , TestLabel "kruskal alg test 1" test_kruskal_1
                 ]
                

main = runTestTT tests
