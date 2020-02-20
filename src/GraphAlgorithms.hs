{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# Language GADTs #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
--{-# Language ConstraintKinds #-}
--{-# Language AllowAmbiguousTypes #-}

-- TODO: replace List with array or vector making appending O(1) instead O(n)

module GraphAlgorithms
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
import PriorityQueue

breadth_first::(Graph g a, Eq a, Hashable a)=>g a->a->[a]
breadth_first g start = iterate queue visited [] where
    queue = [start]
    visited = S.empty
    iterate [] _ r = r
    iterate (vertex:vs) visited res = 
        let visited' = (S.insert vertex visited)
         in if (S.member vertex visited)
            then iterate vs visited res
            else iterate (not_yet_visited vertex visited' vs) visited' (res++[vertex])
    not_yet_visited vertex' visited queue = 
        foldr (\v queue'->if not (S.member v visited)
                          then queue'++[v]
                          else queue'
              ) queue (adjacent_vertices g vertex')


topological_sort::(Graph g a, Eq a, Hashable a)=>g a->[a]
topological_sort g = sortt g queue indegrees [] where
  indegrees = (get_indegrees g) 
  queue = getElems $ filterByProb (==0) indegrees

sortt::(Graph g a, Eq a, Hashable a)=>g a->[a]->PMF Int a->[a]->[a]
sortt g [] indegrees sorted = if length sorted == num_vertices g
                          then sorted
                          else error ("Failure, graph is not acyclical." ++ (show $ length sorted) ++ " / " ++ (show $ num_vertices g))
sortt g (v:qtail) indegrees sorted = 
    let sorted' = sorted++[v]
        adj = adjacent_vertices g v
        (indegrees', queue') = foldr (\v2 (ind, qu) ->
                                      let ind' = decBy 1 v2 ind
                                          qu' = if (getMass ind' v2) == 0 then qu++[v2] else qu
                                       in (ind', qu')
                                   ) (indegrees, qtail) adj
     in sortt g queue' indegrees' sorted'

distance_matrix::(Hashable k, Num a, Ord a, Graph g k, Eq k)=>
     g k  -> k -> HashMap k (a, Maybe k)
distance_matrix g start = iterate initial_dm (start : (M.keys $ M.delete start initial_dm)) where
  initial_dm = foldr (\v acc-> 
                        if (v == start)
                        then (M.insert v (0, Just v) acc)
                        else (M.insert v (-1, Nothing) acc)
                    ) M.empty $ all_nodes g
  iterate dm [] = dm
  iterate dm (v:vs) = iterate dm' vs where
    start_dm = dm M.! v
    start_dist = fst start_dm
    prev = snd start_dm
    dm' = foldr (\v' ->M.adjust (\d -> case d of
                                  (_, Nothing)            -> (start_dist+1, Just v)
                                  (last_dist, last_vertx) -> if last_dist <= start_dist
                                                             then (last_dist, last_vertx)
                                                             else (start_dist+1, Just v)
                                ) v'
                ) dm (adjacent_vertices g v)
 
backtrack::(Show a, Eq a, Hashable a, Applicative t, Foldable t, Monoid (t a))=>a->a->M.HashMap a (d, Maybe a)->t a
backtrack start dest dist_mat = iterate dest (pure dest) where
    iterate dest' res = if dest' == start then res else iterate prev_vert res' where
        prev_dist = dist_mat M.! dest'
        prev_vert = maybe (error ((show dest')++" is not member if dist matrix")) id $ snd prev_dist
        res' = (pure prev_vert) <> res

  

shortest_path_unweighted::(Graph g a, Show a, Eq a, Hashable a, Applicative t, Foldable t, Monoid (t a))=>g a->a->a->t a
shortest_path_unweighted g start dest =
    backtrack start dest $ distance_matrix g start

 

dijkstra_distance_matrix::(WGraph g d a
                          , Graph (g d) a
                          , Ord d
                          , Num d
                          , Show d
                          , Show a
                          , Eq a
                          , Hashable a
                          , Applicative t
                          , Foldable t
                          , Monoid (t a)
                          , PriorityQueue pq t (Infinite d) a
                          ,Show (pq t (Infinite d) a) )=>
                        pq t (Infinite d) a->g d a->a->M.HashMap a (Infinite d, Maybe a)
dijkstra_distance_matrix priorityQueueContructor graph start = 
    iterate initial_dm (Just start,priorityQueueContructor) 
    where
    initial_dm = foldr (\v acc-> 
                          if (v == start)
                          then (M.insert v (Bound 0, Just v)           acc)
                          else (M.insert v (PositiveInfinity, Nothing) acc)
                      ) M.empty $ all_nodes graph
    iterate dm (Nothing, _)  = dm
    iterate dm (Just start, priority_queue)  = iterate dm' (start''', priority_queue'')
      where
      (start'', priority_queue'') = pull_highest_priority_element priority_queue'
      start''' = maybe Nothing (\(s,_)->Just s) start''
      (dm', priority_queue') 
         = foldr (\(v,w) (dm', prq') ->
                    let dist_to_start = (fst $ dm' M.! start)
                        dist = dist_to_start + (Bound w)
                        (old_dist, old_prev) = dm' M.! v
                        (new_dist, new_prev) = if dist < old_dist
                                               then (dist, Just start)
                                               else (old_dist, old_prev)
                        dm'' = M.adjust (const (new_dist, new_prev)) v dm'
                        prq'' = insert_with_priority prq' (v,PositiveInfinity)
                        prq''' = update_weight prq'' v new_dist
                     in (dm'', prq''')
                 ) (dm,  priority_queue) 
                   (adjacent_vertices_weighted graph start)

dijkstra::(PriorityQueue pq t (Infinite d) a
            , WGraph g d a
            , Graph (g d) a
            , Ord d
            , Show a
            , Eq a
            , Hashable a
            , Applicative t
            , Foldable t
            , Monoid (t a)
--            , Monoid d
            , Show d
            ,Show (pq t (Infinite d) a))
        =>pq t (Infinite d) a->g d a->a->a->(Infinite d, t a)
dijkstra priorityQueueContructor graph start dest =
    let dm = dijkstra_distance_matrix priorityQueueContructor graph start
     in (fst $ dm M.! dest, backtrack start dest dm)
