{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# Language GADTs #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
{-# Language RankNTypes #-}

-- TODO: replace List with array or vector making appending O(1) instead O(n)

module AdjacenceMatrix
    where

import Debug.Trace (trace)

import BasicPrelude
import CorePrelude
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Data.Semigroup
import Data.Maybe (fromJust)

import UnweightedGraph
import WeightedGraph

data AdjGraph b a = AdjGraph {getMap::M.HashMap a (M.HashMap a b)} deriving Show

mkAdjGraph = AdjGraph (M.empty)
adjg_num_vertices (AdjGraph g) = M.size g
adjg_adjacent_vertices (AdjGraph g) vertex = S.toList $  M.keysSet (g M.! vertex)
adjg_adjacent_vertices_weighted (AdjGraph g) vertex = M.toList (g M.! vertex)
adjg_add_vertex v (AdjGraph g) = AdjGraph (M.alter (\tryAdj -> Just $ maybe M.empty id tryAdj) v g)
adjg_connect v1 v2 w (AdjGraph g) = AdjGraph (M.adjust (\adj -> M.insert v2 w adj) v1 g)
adjg_add_edge v1 v2 w = adjg_connect v1 v2 w . adjg_add_vertex v1 . adjg_add_vertex v2
adjg_add_edge_undir v1 v2 w = adjg_connect v2 v1 w .adjg_connect v1 v2 w . adjg_add_vertex v1 . adjg_add_vertex v2
adjg_get_weight (AdjGraph g) from to = (M.lookup from g) >>= (\adj->M.lookup to adj)
adjg_get_indegrees (AdjGraph g) = 
  M.foldr (\adj acc ->
              foldr (\v acc'-> M.adjust (+1) v acc') acc $ M.keys adj
          ) (M.map (const 0) g) g
adjg_all_nodes (AdjGraph g) = M.keys g

type UnweightedAdjGraph = AdjGraph Int

instance (Eq a, Hashable a)=>UnweightedGraph UnweightedAdjGraph a where
  emptyGraph           = mkAdjGraph
  num_vertices         = adjg_num_vertices
  adjacent_vertices    = adjg_adjacent_vertices
  add_vertex           = adjg_add_vertex
  connect v1 v2        = undefined v1 v2 1
  add_edge v1 v2       = adjg_add_edge v1 v2 1
  add_edge_undir v1 v2 = adjg_add_edge_undir v1 v2 1
  get_indegrees        = adjg_get_indegrees
  all_nodes            = adjg_all_nodes


instance (Eq a, Hashable a, Ord b)=>WeightedGraph AdjGraph b a where
  emptyGraph                 = mkAdjGraph
  num_vertices               = adjg_num_vertices
  adjacent_vertices          = adjg_adjacent_vertices
  adjacent_vertices_weighted = adjg_adjacent_vertices_weighted
  add_vertex                 = adjg_add_vertex
  connect                    = adjg_connect
  add_edge                   = adjg_add_edge
  add_edge_undir             = adjg_add_edge_undir
  get_weight                 = adjg_get_weight
  get_indegrees              = adjg_get_indegrees
  all_nodes                  = adjg_all_nodes
