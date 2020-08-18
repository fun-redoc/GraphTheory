{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# Language GADTs #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
{-# Language RankNTypes #-}
--{-# Language GeneralizedNewtypeDeriving #-}

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

import qualified Graph as G
import qualified UGraph as UG
import qualified DGraph as DG
import qualified WGraph as WG
import qualified DWGraph as DW
import qualified UWGraph as UW
import Distro

type UnweightedAdjGraph = (AdjGraph PMF Int)
type WeightedAdjGraph a b = (Num b)=>(AdjGraph PMF) b a

data AdjGraph d b a = (Num b, Distro d b a)=>AdjGraph {getMap::M.HashMap a (d b a)}

instance (Show b, Show a, Show (d b a))=>Show (AdjGraph d b a) where
    show (AdjGraph hm) = "\"AM:\"" ++ (show hm)

mkAdjGraph = AdjGraph (M.empty)
adjg_num_vertices (AdjGraph g) = M.size g
adjg_adjacent_vertices (AdjGraph g) vertex = getElems (g M.! vertex)
adjg_adjacent_vertices_weighted (AdjGraph g) vertex = getElemsWithWeights (g M.! vertex)
adjg_add_vertex v (AdjGraph g) = AdjGraph (M.alter (\tryAdj -> Just $ maybe emptyDistro id tryAdj) v g)
adjg_connect v1 v2 w (AdjGraph g) = AdjGraph (M.adjust (\adj -> set v2 w adj) v1 g)
adjg_add_edge v1 v2 w = adjg_connect v1 v2 w . adjg_add_vertex v1 . adjg_add_vertex v2
adjg_add_edge_undir v1 v2 w = adjg_connect v2 v1 w .adjg_connect v1 v2 w . adjg_add_vertex v1 . adjg_add_vertex v2
adjg_get_weight::(Eq a, Hashable a, Num b, Distro d b a)=>(AdjGraph d b a)->a->a->b
adjg_get_weight (AdjGraph g) from to = maybe 0 id $
    (M.lookup from g) >>= (\adj-> return $ getMass adj to)
adjg_get_indegrees (AdjGraph g) = 
  M.foldr (\adj acc -> 
              foldr (\v acc'-> incBy 1 v acc') acc $ getElems adj
          ) nullDistro g
    where nullDistro = foldr (\v distro->set v 0 distro) emptyDistro (M.keys g)
adjg_all_nodes (AdjGraph g) = M.keys g

adjg_all_edges_with_weights::(Eq a, Hashable a, Num b, Distro d b a)=>
                             AdjGraph d b a->[((a,a),b)]
adjg_all_edges_with_weights (AdjGraph g) 
    = M.foldrWithKey 
        (\v neighbours edges->
          edges++(map (\(v',w)->((v,v'),w)) $ getElemsWithWeights neighbours)
        ) [] g

adjg_all_edges::(Eq a, Hashable a, Num b, Distro d b a)=>AdjGraph d b a->[(a,a)]
adjg_all_edges(AdjGraph g) 
    = M.foldrWithKey 
        (\v neighbours edges->
          edges++(map (\v'->((v,v'))) $ getElems neighbours)
        ) [] g
adjg_num_edges = length . adjg_all_edges

instance (Eq a, Hashable a, Ord b, Distro d b a)=>G.Graph (AdjGraph d b) a where
  emptyGraph                 = mkAdjGraph
  num_vertices               = adjg_num_vertices
  adjacent_vertices          = adjg_adjacent_vertices
  add_vertex                 = adjg_add_vertex
  all_nodes                  = adjg_all_nodes

instance (Eq a, Hashable a, Num b, Distro d b a)=>UG.UGraph (AdjGraph d b) a where
  connect v1 v2 g  = adjg_connect v2 v1 0 $ adjg_connect v1 v2 0 g
  add_edge v1 v2 g = adjg_add_edge v2 v1 0 $ adjg_add_edge v1 v2 0 g
  num_edges                  = (`quot` 2) . adjg_num_edges
  all_edges                  = (nubBy (\(v1,v2) (v1',v2')-> -- make undir
                                            (v1 == v1' && v2 == v2')
                                         || (v1 == v2' && v2 == v1')
                                      )
                               )
                               . adjg_all_edges

instance (Eq a, Hashable a, Num b, Distro d b a)=>DG.DGraph (AdjGraph d b) a where
  connect v1 v2 g  = adjg_connect v1 v2 0 g
  add_edge v1 v2 g = adjg_add_edge v1 v2 0 g
  num_edges        = adjg_num_edges
  all_edges        = adjg_all_edges
  get_indegrees    = adjg_get_indegrees

instance (Num b, Eq a, Hashable a, Ord b, Distro d b a)=>WG.WGraph (AdjGraph d) b a where
  get_weight                 = adjg_get_weight
  adjacent_vertices          = adjg_adjacent_vertices_weighted

instance (Num b, Eq a, Hashable a, Ord b, Distro d b a)=>DW.DWGraph (AdjGraph d) b a where
  connect  v1 v2 w = adjg_connect v1 v2 w 
  add_edge v1 v2 w = adjg_add_edge v1 v2 w
  all_edges        = adjg_all_edges_with_weights

instance (Num b, Eq a, Hashable a, Ord b, Distro d b a)=>UW.UWGraph (AdjGraph d) b a where
  connect v1 v2 w  = adjg_connect v2 v1 w . adjg_connect v1 v2 w
  add_edge v1 v2 w = adjg_add_edge v2 v1 w . adjg_add_edge v1 v2 w
--  num_edges                  = (`quot` 2) . adjg_num_edges
  all_edges                  = (nubBy (\((v1,v2),_) ((v1',v2'),_)-> -- make undir
                                            (v1 == v1' && v2 == v2')
                                         || (v1 == v2' && v2 == v1')
                                      )
                               )
                               . adjg_all_edges_with_weights
