{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# Language GADTs #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}

-- TODO: replace List with array or vector making appending O(1) instead O(n)

module UWGraph where

import Debug.Trace (trace)

import BasicPrelude
import CorePrelude
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Data.Maybe (fromJust)

import qualified Graph as G
import qualified UGraph as UG
import qualified WGraph as WG
import Distro

class (G.Graph (g p) a, UG.UGraph (g p) a, WG.WGraph g p a)=>UWGraph g p a where
  add_edge::a->a->p->g p a->g p a
  connect::a->a->p->g p a->g p a
  all_edges::g p a->[((a,a),p)]
