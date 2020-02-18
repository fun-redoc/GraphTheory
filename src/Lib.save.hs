{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# Language GADTs #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}

-- TODO: replace List with array or vector making appending O(1) instead O(n)

module Lib
--    ( someFunc
--    ) 
    where

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
instance (Show a, Semigroup a)=>Semigroup (Infinite a) where
    (<>) NegativeInfinity (Bound _) = NegativeInfinity
    (<>) PositiveInfinity (Bound x) = PositiveInfinity
    (<>) (Bound _) PositiveInfinity = PositiveInfinity
    (<>) (Bound _) NegativeInfinity = NegativeInfinity
    -- undefined (<>) PositiveInfinity NegativeInfinity = ??
    -- undefined (<>) NegativeInfinity PositiveInfinity = ??
    (<>) (Bound x) (Bound y) = Bound (x <> y)
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

class Graph g a b where
  num_vertices::g a b->Int
  adjacent_vertices::g a b->a->[a]
  adjacent_vertices_weighted::g a b->a->[(a,b)]
  add_vertex::a->g a b->g a b
  connect::a->a->b->g a b->g a b
  add_edge::a->a->b->g a b->g a b
  add_edge_undir::a->a->b->g a b->g a b
  get_weight::g a b->a->a->Maybe b
  get_indegrees::g a b->M.HashMap a Int
  all_nodes::g a b->[a]

data AdjGraph a b = AdjGraph {getMap::M.HashMap a (M.HashMap a b)} deriving Show
mkAdjGraph = AdjGraph (M.empty)
instance (Eq a, Hashable a, Ord b)=> Graph AdjGraph a b where
  num_vertices (AdjGraph g) = M.size g
  adjacent_vertices (AdjGraph g) vertex = S.toList $  M.keysSet (g M.! vertex)
  adjacent_vertices_weighted (AdjGraph g) vertex = M.toList (g M.! vertex)
  add_vertex v (AdjGraph g) = AdjGraph (M.alter (\tryAdj -> Just $ maybe M.empty id tryAdj) v g)
  connect v1 v2 w (AdjGraph g) = AdjGraph (M.adjust (\adj -> M.insert v2 w adj) v1 g)
  add_edge v1 v2 w = connect v1 v2 w . add_vertex v1 . add_vertex v2
  add_edge_undir v1 v2 w = connect v2 v1 w .connect v1 v2 w . add_vertex v1 . add_vertex v2
  get_weight (AdjGraph g) from to = (M.lookup from g) >>= (\adj->M.lookup to adj)
  get_indegrees (AdjGraph g) = 
    M.foldr (\adj acc ->
                foldr (\v acc'-> M.adjust (+1) v acc') acc $ M.keys adj
            ) (M.map (const 0) g) g
  all_nodes (AdjGraph g) = M.keys g



breadth_first::(Graph g a b, Eq a, Hashable a)=>g a b->a->[a]
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


topological_sort::(Graph g a b, Eq a, Hashable a)=>g a b->[a]
topological_sort g = sortt queue indegrees [] where
  indegrees = get_indegrees g
  queue = M.keys $ M.filter (==0) indegrees
  sortt [] indegrees sorted = if length sorted == num_vertices g
                              then sorted
                              else error ("Failure, graph is not acyclical." ++ (show $ length sorted) ++ " / " ++ (show $ num_vertices g))
  sortt (v:qtail) indegrees sorted = 
    let sorted' = sorted++[v]
        adj = adjacent_vertices g v
        (indegrees', queue') = foldr (\v2 (ind, qu) ->
                                      let ind' = M.adjust (\i->i-1) v2 ind
                                          qu' = if ind' M.! v2 == 0 then qu++[v2] else qu
                                       in (ind', qu')
                                   ) (indegrees, qtail) adj
     in sortt queue' indegrees' sorted'

distance_matrix::(Hashable k, Num a, Ord a, Graph g k b, Eq k)=>
     g k b -> k -> HashMap k (a, Maybe k)
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

  

shortest_path_unweighted::(Graph g a d, Show a, Eq a, Hashable a, Applicative t, Foldable t, Monoid (t a))=>g a d->a->a->t a
shortest_path_unweighted g start dest =
    backtrack start dest $ distance_matrix g start

 

dijkstra_distance_matrix::(Graph g a d, Monoid d, Show d, Show a, Eq a, Hashable a, Applicative t, Foldable t, Monoid (t a), PriorityQueue pq t (Infinite d) a,Show (pq t (Infinite d) a) )=>pq t (Infinite d) a->g a d->a->M.HashMap a (Infinite d, Maybe a)
dijkstra_distance_matrix pq g start = iterate initial_dm  
                                              (Just start,pq)  where
  initial_dm = foldr (\v acc-> 
                        if (v == start)
                        then (M.insert v (Bound mempty, Just v)           acc)
                        else (M.insert v (PositiveInfinity, Nothing) acc)
                    ) M.empty $ all_nodes g
  iterate dm (maybe_start, priority_queue)  = if maybe_start == Nothing 
                                              then dm  
                                              else iterate dm' 
                                                           (start''', priority_queue'')
                                              where
    start = fromJust maybe_start
    (start'', priority_queue'') = pull_highest_priority_element priority_queue'
    start''' = maybe Nothing (\(s,_)->Just s) start''
    (dm', priority_queue') = foldr (\(v,w) (dm', prq') ->
                                      let dist = (fst $ dm' M.! start) <> (Bound w)
                                          (old_dist, old_prev) = dm' M.! v
                                          (new_dist, new_prev) = if dist < old_dist
                                                                 then (dist, Just start)
                                                                 else (old_dist, old_prev)
                                          dm'' = M.adjust (const (new_dist, new_prev)) v dm'
                                          prq'' = insert_with_priority prq' (v,PositiveInfinity)
                                          prq''' = update_weight prq'' v new_dist
                                       in (dm'', prq''')
                                   ) (dm,  priority_queue) (adjacent_vertices_weighted g start)

dijkstra::(PriorityQueue pq t (Infinite d) a, Graph g a d, Show a, Eq a, Hashable a, Applicative t, Foldable t, Monoid (t a), Monoid d, Show d,Show (pq t (Infinite d) a))=>pq t (Infinite d) a->g a d->a->a->(Infinite d, t a)
dijkstra pq g start dest =
    let dm = dijkstra_distance_matrix pq g start
     in (fst $ dm M.! dest, backtrack start dest dm)


someFunc :: IO ()
someFunc = do
    let ugraph =  add_edge_undir (0::Int) 1 1 
                . add_edge_undir 1 2 1
                . add_edge_undir 2 7 1
                . add_edge_undir 2 4 1
                . add_edge_undir 2 3 1
                . add_edge_undir 1 5 1
                . add_edge_undir 5 6 1
                . add_edge_undir 3 6 1
                . add_edge_undir 3 4 1
                . add_edge_undir 6 8 1
                $ mkAdjGraph
    let dgraph =  add_edge (0::Int) 1 1 
                . add_edge 1 2 1
                . add_edge 2 7 1
                . add_edge 2 4 1
                . add_edge 2 3 1
                . add_edge 1 5 1
                . add_edge 5 6 1
                . add_edge 3 6 1
                . add_edge 3 4 1
                . add_edge 6 8 1
                $ mkAdjGraph
    putStrLn $ tshow $ breadth_first ugraph 2
    putStrLn $ tshow $ get_indegrees dgraph
    putStrLn $ tshow $ topological_sort dgraph
    putStrLn "---------------------------------"
    let ugraph1 = add_edge_undir(0::Int) 1 1
                . add_edge_undir 1 2 1
                . add_edge_undir 1 3 1
                . add_edge_undir 2 3 1
                . add_edge_undir 1 4 1
                . add_edge_undir 3 5 1
                . add_edge_undir 5 4 1
                . add_edge_undir 3 6 1
                . add_edge_undir 6 7 1
                . add_edge_undir 0 7 1
                $ mkAdjGraph
    putStrLn $ tshow $ ((shortest_path_unweighted ugraph1 0 5)::[Int])
    putStrLn $ tshow $ ((shortest_path_unweighted ugraph1 0 6)::[Int])
    putStrLn $ tshow $ ((shortest_path_unweighted ugraph1 7 4)::[Int])
    putStrLn "---------------------------------"
    let dgraph1 = add_edge(0::Int) 1 1
                . add_edge 1 2 1
                . add_edge 1 3 1
                . add_edge 2 3 1
                . add_edge 1 4 1
                . add_edge 3 5 1
                . add_edge 5 4 1
                . add_edge 3 6 1
                . add_edge 6 7 1
                . add_edge 0 7 1
                $ mkAdjGraph
    putStrLn $ tshow $ ((shortest_path_unweighted dgraph1 0 5)::[Int])
    putStrLn $ tshow $ ((shortest_path_unweighted dgraph1 0 6)::[Int])
    --putStrLn $ tshow $ ((shortest_path_unweighted ugraph1 7 4)::[Int])
    putStrLn "---------------------------------"
    let dwgraph1 = add_edge 'a' 'b' (2::Int)
                 . add_edge 'a' 'c' 3
                 . add_edge 'b' 'd' 2
                 . add_edge 'c' 'e' 6
                 . add_edge 'e' 'b' 5
                 . add_edge 'e' 'd' 4
                 $ mkAdjGraph
    --putStrLn $ tshow $ ((dijkstra_distance_matrix (emptyPriorityQueue::TrivialPQ (Infinite Int) Char) dwgraph1 'a'))
    putStrLn $ tshow $ ((dijkstra (emptyPriorityQueue::TrivialPQ (Infinite Int) Char) dwgraph1 'a' 'd')::(Infinite Int, [Char]))
    putStrLn $ tshow $ ((dijkstra (emptyPriorityQueue::TrivialPQ (Infinite Int) Char) dwgraph1 'a' 'e')::(Infinite Int, [Char]))
    putStrLn $ tshow $ ((dijkstra (emptyPriorityQueue::TrivialPQ (Infinite Int) Char) dwgraph1 'a' 'b')::(Infinite Int, [Char]))
--    putStrLn $ tshow $ ((dijkstra (emptyPriorityQueue::TrivialPQ (Infinite Int) Char) dwgraph1 'd' 'a')::(Infinite Int, [Char]))
    putStrLn $ tshow $ ((dijkstra (emptyPriorityQueue::TrivialPQ (Infinite Int) Int) dgraph1 0 6)::(Infinite Int, [Int]))
