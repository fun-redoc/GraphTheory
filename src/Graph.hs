{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# Language GADTs #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}

-- TODO: replace List with array or vector making appending O(1) instead O(n)

module Graph where

import Debug.Trace (trace)

import BasicPrelude
import CorePrelude
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
--import Data.Semigroup
import Data.Maybe (fromJust)


class Graph g a  where
  emptyGraph       ::g a
  num_vertices     ::g a ->Int
  adjacent_vertices::g a ->a->[a]
  add_vertex       ::a->g a ->g a 
  connect          ::a->a->g a->g a
  add_edge         ::a->a->g a ->g a
  add_edge_undir   ::a->a->g a->g a
  get_indegrees    ::(Distro d Int a)=>g a->(d Int a)
  all_nodes        ::g a->[a]
  all_edges        ::g a->[(a,a)]


class (Num p)=>WGraph g p a where
  get_weight                ::g p a->a->a->p
  adjacent_vertices_weighted::g p a->a->[(a,p)]
  connect_weighted          ::a->a->p->g p a->g p a
  add_edge_weighted         ::a->a->p->g p a->g p a
  add_edge_weighted_undir   ::a->a->p->g p a->g p a
  all_edges_weighted        ::g p a->[((a,a),p)]


class (Num p, Ord p)=>Distro d p a where
    emptyDistro::d p a
    filterByProb::(p->Bool)->d p a->d p a
    getMass::d p a->a->p
    getElems::d p a->[a]
    getElemsWithWeights::d p a->[(a,p)]
    set::a->p->d p a->d p a
    incBy::p->a->d p a->d p a
    decBy::p->a->d p a->d p a


data PMF p a = PMF (M.HashMap a p)
instance (Eq a, Hashable a, Num p, Ord p)=>Distro PMF p a where
  emptyDistro = PMF $ M.empty
  filterByProb fn (PMF hm) = PMF $ M.filter fn hm
  getMass (PMF hm) e = maybe 0 id $ M.lookup e hm
  getElems (PMF hm) = M.keys hm
  getElemsWithWeights (PMF hm) = M.toList hm
  set   e p (PMF hm) = PMF $ M.alter (const $ Just p) e hm
  incBy p e (PMF hm) = PMF $ M.alter (\maybe_p->Just $ maybe p (\p'->p'+p) maybe_p) e hm
  decBy p e d        = incBy (-1) e d
instance (Show a, Show p)=>Show (PMF p a) where
    show (PMF hm) = "PMF: "++(show hm)

