{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# Language GADTs #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}

-- TODO: replace List with array or vector making appending O(1) instead O(n)

module Distro where

import Debug.Trace (trace)

import BasicPrelude
import CorePrelude
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
--import Data.Semigroup
import Data.Maybe (fromJust)

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

