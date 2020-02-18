{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# Language GADTs #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}

-- TODO: replace List with array or vector making appending O(1) instead O(n)

module WeightedGraph where

import Debug.Trace (trace)

import BasicPrelude
import CorePrelude
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Data.Semigroup
import Data.Maybe (fromJust)

class WeightedGraph g b a where
  emptyGraph                ::g b a
  num_vertices              ::g b a->Int
  adjacent_vertices         ::g b a->a->[a]
  adjacent_vertices_weighted::g b a->a->[(a,b)]
  add_vertex                ::a->g b a->g b a
  connect                   ::a->a->b->g b a->g b a
  add_edge                  ::a->a->b->g b a->g b a
  add_edge_undir            ::a->a->b->g b a->g b a
  get_weight                ::g b a->a->a->Maybe b
  get_indegrees             ::g b a->M.HashMap a Int
  all_nodes                 ::g b a->[a]

