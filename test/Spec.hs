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

-- the main
main = runTestTT tests

-- the test suite
tests = TestList [ TestLabel "prims alg test 1" test_prims_1
                 , TestLabel "prims alg test 2" test_prims_2 
                 , TestLabel "kruskal alg test 1" test_kruskal_1
                 , TestLabel "kruskal alg test 2" test_kruskal_2
                 ]

-- the data for tests
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
uwgraph3 = UW.add_edge 0 1 1
         . UW.add_edge 1 2 2
         . UW.add_edge 1 3 2
         . UW.add_edge 2 3 2
         . UW.add_edge 1 4 3
         . UW.add_edge 3 5 1
         . UW.add_edge 5 4 2
         . UW.add_edge 3 6 1
         . UW.add_edge 6 7 1
         . UW.add_edge 7 0 1
         $ G.emptyGraph::(WeightedAdjGraph Int Int)

-- the testcases
test_prims_1 = 
  TestCase ( do let priorityQueueConstructor = (emptyPriorityQueue::(TrivialPQ (Char,Char) Int))
                let tree = ((prims priorityQueueConstructor uwgraph1 'e')::(WeightedAdjGraph Char Int)) 
                assertEqual "spanning tree and original graph must have same number of verticies" 
                            (G.num_vertices uwgraph1) (G.num_vertices tree)
                assertEqual "spanning tree has no cycles"
                            ((UG.num_edges tree) == ((G.num_vertices tree)-1)) True
                putStrLn "\n"
--                putStrLn $ tshow $ tree
           )
test_prims_2 = 
  TestCase ( do let priorityQueueConstructor = (emptyPriorityQueue::(TrivialPQ (Int,Int) Int))
                let tree = ((prims priorityQueueConstructor uwgraph2 1)::(WeightedAdjGraph Int Int)) 
                assertEqual "spanning tree and original graph must have same number of verticies"
                            (G.num_vertices uwgraph2) (G.num_vertices tree)
                assertEqual "spanning tree has no cycles"
                            ((UG.num_edges tree) == ((G.num_vertices tree)-1)) True
                putStrLn "\n"
--                putStrLn $ tshow $ tree
           )
test_kruskal_1 = 
  TestCase ( do let priorityQueueConstructor = (emptyPriorityQueue::(TrivialPQ (Char,Char) Int))
                let tree = ((kruskal priorityQueueConstructor uwgraph1)::(WeightedAdjGraph Char Int)) 
                assertEqual "spanning tree and original graph must have same number of verticies" 
                            (G.num_vertices uwgraph1) (G.num_vertices tree)
                assertEqual "spanning tree has no cycles"
                            ((UG.num_edges tree) == ((G.num_vertices tree)-1)) True
                putStrLn "\n"
--                putStrLn $ tshow $ tree
           )
test_kruskal_2 = 
  TestCase ( do let priorityQueueConstructor = (emptyPriorityQueue::(TrivialPQ (Int,Int) Int))
                let tree = ((kruskal priorityQueueConstructor uwgraph3)::(WeightedAdjGraph Int Int)) 
                assertEqual "spanning tree and original graph must have same number of verticies" 
                            (G.num_vertices uwgraph3) (G.num_vertices tree)
                assertEqual "spanning tree has no cycles"
                            ((UG.num_edges tree) == ((G.num_vertices tree)-1)) True
                putStrLn "\n"
                putStrLn $ tshow $ tree
           )

