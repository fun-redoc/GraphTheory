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
import Data.Maybe (fromJust)

import Distro


class Graph g a  where
  emptyGraph       ::g a
  num_vertices     ::g a ->Int
  adjacent_vertices::g a ->a->[a]
  all_nodes        ::g a->[a]
  add_vertex       ::a->g a ->g a 
