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

import Graph


type UnweightedAdjGraph = (AdjGraph PMF Int)
type WeightedAdjGraph = (AdjGraph PMF) 


data AdjGraph d b a = (Num b, Distro d b a)=>AdjGraph {getMap::M.HashMap a (d b a)}

instance (Show b, Show a, Show (d b a))=>Show (AdjGraph d b a) where
    show (AdjGraph hm) = "AM: "++(show hm)

mkAdjGraph = AdjGraph (M.empty)
adjg_num_vertices (AdjGraph g) = M.size g
adjg_adjacent_vertices (AdjGraph g) vertex = getElems (g M.! vertex)
adjg_adjacent_vertices_weighted (AdjGraph g) vertex = getElemsWithWeights (g M.! vertex)
adjg_add_vertex v (AdjGraph g) = AdjGraph (M.alter (\tryAdj -> Just $ maybe emptyDistro id tryAdj) v g)
adjg_connect v1 v2 w (AdjGraph g) = AdjGraph (M.adjust (\adj -> set v2 w adj) v1 g)
adjg_add_edge v1 v2 w = adjg_connect v1 v2 w . adjg_add_vertex v1 . adjg_add_vertex v2
adjg_add_edge_undir v1 v2 w = adjg_connect v2 v1 w .adjg_connect v1 v2 w . adjg_add_vertex v1 . adjg_add_vertex v2
adjg_get_weight (AdjGraph g) from to = maybe 0 id $
    (M.lookup from g) >>= (\adj-> return $ getMass adj to)
adjg_get_indegrees (AdjGraph g) = 
  M.foldr (\adj acc -> 
              foldr (\v acc'-> incBy 1 v acc') acc $ getElems adj
          ) nullDistro g
    where nullDistro = foldr (\v distro->set v 0 distro) emptyDistro (M.keys g)
adjg_all_nodes (AdjGraph g) = M.keys g



instance (Eq a, Hashable a, Ord b, Distro d b a)=>Graph (AdjGraph d b) a where
  emptyGraph                 = mkAdjGraph
  num_vertices               = adjg_num_vertices
  adjacent_vertices          = adjg_adjacent_vertices
  add_vertex                 = adjg_add_vertex
  connect v1 v2              = adjg_connect v1 v2 0
  add_edge v1 v2             = adjg_add_edge v1 v2 0
  add_edge_undir v1 v2       = adjg_add_edge_undir v1 v2 0
  get_indegrees              = adjg_get_indegrees
  all_nodes                  = adjg_all_nodes

instance (Num b, Eq a, Hashable a, Ord b, Distro d b a)=>
         WGraph (AdjGraph d b) a b where
  get_weight                 = adjg_get_weight
  adjacent_vertices_weighted = adjg_adjacent_vertices_weighted
  connect_weighted v1 v2 w = adjg_connect v1 v2 w 
  add_edge_weighted v1 v2 w = adjg_add_edge v1 v2 w
  add_edge_weighted_undir v1 v2 w = adjg_add_edge_undir v1 v2 w
