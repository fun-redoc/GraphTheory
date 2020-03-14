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

import qualified Graph as G
import qualified UGraph as UG
import qualified DGraph as DG
import qualified WGraph as WG
import qualified DWGraph as DW
import qualified UWGraph as UW
import Distro
import PriorityQueue

breadth_first::(G.Graph g a, Eq a, Hashable a)=>g a->a->a->Maybe [a]
breadth_first g start dest  = iterate queue visited [] where
    queue = [start]
    visited = S.empty
    iterate [] _ _ = Nothing
    iterate (vertex:vs) visited res 
        | vertex == dest = Just (res++[dest]) 
        | otherwise = 
            let visited' = (S.insert vertex visited)
             in if (S.member vertex visited)
                then iterate vs visited res
                else iterate (not_yet_visited vertex visited' vs) visited' (res++[vertex])
    not_yet_visited vertex' visited queue = 
        foldr (\v queue'->if not (S.member v visited)
                          then queue'++[v]
                          else queue'
              ) queue (G.adjacent_vertices g vertex')


topological_sort::(G.Graph g a, DG.DGraph g a, Eq a, Hashable a)=>g a->Maybe [a]
topological_sort g = sortt g queue indegrees [] where
  indegrees = (DG.get_indegrees g) 
  queue = getElems $ filterByProb (==0) indegrees

sortt::(G.Graph g a, DG.DGraph g a, Eq a, Hashable a)=>g a->[a]->PMF Int a->[a]->Maybe [a]
sortt g [] indegrees sorted = if length sorted == G.num_vertices g
                          then Just sorted
                          else Nothing -- error ("Failure, graph is not acyclical." ++ (show $ length sorted) ++ " / " ++ (show $ num_vertices g))
sortt g (v:qtail) indegrees sorted = 
    let sorted' = sorted++[v]
        adj = G.adjacent_vertices g v
        (indegrees', queue') = foldr (\v2 (ind, qu) ->
                                      let ind' = decBy 1 v2 ind
                                          qu' = if (getMass ind' v2) == 0 then qu++[v2] else qu
                                       in (ind', qu')
                                   ) (indegrees, qtail) adj
     in sortt g queue' indegrees' sorted'

distance_matrix::(Hashable k, Num a, Ord a, G.Graph g k, Eq k)=>
     g k  -> k -> HashMap k (a, Maybe k)
distance_matrix g start = iterate initial_dm (start : (M.keys $ M.delete start initial_dm)) where
  initial_dm = foldr (\v acc-> 
                        if (v == start)
                        then (M.insert v (0, Just v) acc)
                        else (M.insert v (-1, Nothing) acc)
                    ) M.empty $ G.all_nodes g
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
                ) dm (G.adjacent_vertices g v)
 
backtrack::(Show a, Eq a, Hashable a, Applicative t, Foldable t, Monoid (t a))=>a->a->M.HashMap a (d, Maybe a)->t a
backtrack start dest dist_mat = iterate dest (pure dest) where
    iterate dest' res = if dest' == start then res else iterate prev_vert res' where
        prev_dist = dist_mat M.! dest'
        prev_vert = maybe (error ((show dest')++" is not member if dist matrix")) id $ snd prev_dist
        res' = (pure prev_vert) <> res

  

shortest_path_unweighted::(G.Graph g a, Show a, Eq a, Hashable a, Applicative t, Foldable t, Monoid (t a))=>g a->a->a->t a
shortest_path_unweighted g start dest =
    backtrack start dest $ distance_matrix g start

 

dijkstra_distance_matrix::( WG.WGraph g d a
                          , G.Graph (g d) a
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
                      ) M.empty $ G.all_nodes graph
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
                   (WG.adjacent_vertices graph start)

dijkstra::(PriorityQueue pq t (Infinite d) a
            , WG.WGraph g d a
            , G.Graph (g d) a
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


prims::( PriorityQueue pq t1 d (a,a) -- to be able to use several implementaitons of priority queue, an efficient implementation may be important for efficiency of prims alg.
       , Show (pq t1 d (a,a))
       , UW.UWGraph g d a
       , WG.WGraph g d a
       , G.Graph (g d) a
       , Num d, Ord d
       , Hashable a, Eq a
       , Show a, Show d
       , UW.UWGraph t d a
       , WG.WGraph t d a
       , UG.UGraph (t d) a
       , G.Graph (t d) a
       )
       =>pq t1 d (a,a)->g d a->a->t d a
prims priorityQueueConstructor graph start_vertex 
      = build_spann_tree priority_queue' S.empty G.emptyGraph
 where
 priority_queue' = foldl (\pq (v,w)->insert_with_priority pq ((start_vertex,v),w)) 
                         priorityQueueConstructor  
                         $ WG.adjacent_vertices graph start_vertex
 build_spann_tree  priority_queue already_visited tmp_spann_tree =
   if is_empty priority_queue
   then -- ready
        tmp_spann_tree
   else -- next alg iteration
        if vertex' `S.member` already_visited
        then -- drop if alread visited
             build_spann_tree priority_queue'  already_visited   tmp_spann_tree 
        else -- take if not visited
             build_spann_tree priority_queue'' already_visited'' tmp_spann_tree' 
   where
    (maybe_edge, priority_queue') = pull_highest_priority_element priority_queue
    ((vertex, vertex'), weight)   = fromJust maybe_edge -- if statement makes shure Nothing cannot happen
    already_visited'              = vertex  `S.insert` already_visited
    already_visited''             = vertex' `S.insert` already_visited'
    adjacence                     = WG.adjacent_vertices graph vertex'
    priority_queue''              = foldl (\pq (v,w)->
                                             insert_with_priority pq ((vertex',v),w)
                                          ) priority_queue' adjacence
    tmp_spann_tree'               = UW.add_edge vertex vertex' weight tmp_spann_tree
                       
    
    
kruskal::( PriorityQueue pq t1 d (a,a) -- to be able to use several implementaitons of priority queue, an efficient implementation may be important for efficiency of prims alg.
       , Show (pq t1 d (a,a))
       , UW.UWGraph g d a
       , UG.UGraph (t d) a
       , WG.WGraph g d a
       , G.Graph (g d) a
       , Num d, Ord d
       , Hashable a, Eq a, Ord a
       , Hashable (a,a)
       , Show a, Show d
       , UW.UWGraph t d a
       , WG.WGraph t d a
       , UG.UGraph (t d) a
       , G.Graph (t d) a
       )
       =>pq t1 d (a,a)->g d a->t d a
kruskal priorityQueueConstructor graph = build_tree sorted_edges 0 G.emptyGraph where
    sorted_edges = foldl (\pq ((v1,v2),w) -> insert_with_priority pq (((min v1 v2), (max v1 v2)),w))
                         priorityQueueConstructor $ UW.all_edges graph
    n            = (G.num_vertices graph) - 1
    build_tree priority_queue num_edges tmp_tree
      |  n == num_edges = tmp_tree
      |  otherwise      = let (shortest_edge, priority_queue') = pull_highest_priority_element priority_queue
                              maybe_tmp_tree' = shortest_edge 
                                                >>= (\((v,v'),w)->return $ UW.add_edge v v' w tmp_tree)
                              tmp_tree'       = fromJust maybe_tmp_tree'
                              num_edges'      = num_edges + 1
                              has_cycle = (num_edges' > ((G.num_vertices tmp_tree')-1))
                           in if has_cycle
                              then -- ignore this edge, because a cycle would be introduced, 
                                   -- or no more edges left
--                                   (trace $ show ("here 1", shortest_edge, has_cycle,2*num_edges',((num_vertices tmp_tree')-1))) $
                                   build_tree priority_queue' num_edges tmp_tree
                              else -- continue building
--                                   (trace$ show("here 2", shortest_edge,num_edges',((num_vertices tmp_tree')))) $
                                   build_tree priority_queue' num_edges' tmp_tree'
