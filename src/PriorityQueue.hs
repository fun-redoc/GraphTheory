{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# Language GADTs #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}

-- TODO: replace List with array or vector making appending O(1) instead O(n)

module PriorityQueue where

import Debug.Trace (trace)

import BasicPrelude
import CorePrelude
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Data.Semigroup
import Data.Maybe (fromJust)

instance Semigroup Int where
    (<>) = (+)
instance Monoid Int where
    mempty = 0

data Infinite a = NegativeInfinity | Bound a | PositiveInfinity deriving (Ord, Show, Eq)
instance (Ord a, Show a, Num a)=>Num (Infinite a) where
    (+) (Bound x) (Bound y) = Bound (x + y)
    (+) NegativeInfinity (Bound _) = NegativeInfinity
    (+) PositiveInfinity (Bound _) = PositiveInfinity
    (+) (Bound _) PositiveInfinity = PositiveInfinity
    (+) (Bound _) NegativeInfinity = NegativeInfinity
    -- undefined (<>) PositiveInfinity NegativeInfinity = ??
    -- undefined (<>) NegativeInfinity PositiveInfinity = ??
    (+) x y = error ((show x)++"+"++(show y)++" is not defined")
    negate (Bound x) = Bound (negate x)
    negate PositiveInfinity = NegativeInfinity
    negate NegativeInfinity = PositiveInfinity
    (*) (Bound x) (Bound y) = Bound (x * y)
    (*) NegativeInfinity (Bound x) = if x > 0 then NegativeInfinity else PositiveInfinity
    (*) PositiveInfinity (Bound x) = if x > 0 then PositiveInfinity else NegativeInfinity
    (*) (Bound x) PositiveInfinity = if x > 0 then PositiveInfinity else NegativeInfinity
    (*) (Bound x) NegativeInfinity = if x > 0 then NegativeInfinity else PositiveInfinity
    -- undefined (<>) PositiveInfinity NegativeInfinity = ??
    -- undefined (<>) NegativeInfinity PositiveInfinity = ??
    (*) x y = error ((show x)++"*"++(show y)++" is not defined")
    abs (Bound x) = Bound (abs x)
    abs NegativeInfinity = PositiveInfinity
    abs PositiveInfinity = PositiveInfinity
    signum (Bound x) = Bound (signum x)
    signum PositiveInfinity = Bound (fromInteger 1)
    signum NegativeInfinity = Bound (fromInteger (-1))
    fromInteger x = Bound (fromInteger x)

instance (Show a, Semigroup a)=>Semigroup (Infinite a) where
    (<>) (Bound x) (Bound y) = Bound (x <> y)
    (<>) NegativeInfinity (Bound _) = NegativeInfinity
    (<>) PositiveInfinity (Bound x) = PositiveInfinity
    (<>) (Bound _) PositiveInfinity = PositiveInfinity
    (<>) (Bound _) NegativeInfinity = NegativeInfinity
    -- undefined (<>) PositiveInfinity NegativeInfinity = ??
    -- undefined (<>) NegativeInfinity PositiveInfinity = ??
    (<>) x y = error ((show x)++"<>"++(show y)++" is not defined")
instance (Show a, Monoid a)=>Monoid (Infinite a) where
    mempty = (Bound (mempty::a))

class (Applicative t, Foldable t, Ord p)=>PriorityQueue pq t p a where
    emptyPriorityQueue::pq t p a
    is_empty::pq t p a->Bool
    insert_with_priority::pq t p a->(a,p)->pq t p a
    pull_highest_priority_element::pq t p a->(Maybe (a,p), pq t p a)
    update_weight::pq t p a->a->p->pq t p a

data PQ t p a = PQ (t (a,p)) 
instance (Show a, Eq a,  Ord p)=>PriorityQueue PQ [] p a where
    emptyPriorityQueue = PQ []
    is_empty (PQ pq) = null pq
    insert_with_priority (PQ pq) (e,p) = PQ $ nubBy (\(x,_) (y,_) -> if x /= y 
                                                                 then False 
                                                                 else True)
                                            $ reverse 
                                            $ sortOn snd ((e,p):pq)
    pull_highest_priority_element pq@(PQ [])= (Nothing, pq)
    pull_highest_priority_element (PQ (x:xs))= (Just x, PQ xs)
    update_weight (PQ xs) x p = PQ $ sortOn snd $ map (\(x',p') -> if x == x' then (x,p) else (x', p')) xs
instance (Show p, Show a)=>Show (PQ [] p a) where
    show (PQ pq) = show pq

type TrivialPQ a p = PQ [] a p
