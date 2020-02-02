{-# Language GADTs #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language ScopedTypeVariables #-}

-- TODO: replace List with array or vector making appending O(1) instead O(n)

module Lib
    ( someFunc
    ) where

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Hashable (Hashable)

class Graph g a b where
  num_vertices::g a b->Int
  adjacent_vertices::g a b->a->[a]
  add_vertex::a->g a b->g a b
  connect::a->a->b->g a b->g a b
  add_edge::a->a->b->g a b->g a b
  add_edge_undir::a->a->b->g a b->g a b
  get_indegrees::g a b->M.HashMap a Int

data AdjGraph a b = AdjGraph {getMap::M.HashMap a (M.HashMap a b)} deriving Show
mkAdjGraph = AdjGraph (M.empty)
instance (Eq a, Hashable a, Ord b)=> Graph AdjGraph a b where
  num_vertices (AdjGraph g) = M.size g
  adjacent_vertices (AdjGraph g) vertex = S.toList $  M.keysSet (g M.! vertex)
  add_vertex v (AdjGraph g) = AdjGraph (M.alter (\tryAdj -> Just $ maybe M.empty id tryAdj
--                                                    case tryAdj of
--                                                        Nothing->Just M.empty 
--                                                        Just adj->Just adj
                                                ) v g)
  connect v1 v2 w (AdjGraph g) = AdjGraph (M.adjust (\adj -> M.insert v2 w adj) v1 g)
  add_edge v1 v2 w = connect v1 v2 w . add_vertex v1 . add_vertex v2
  add_edge_undir v1 v2 w = connect v2 v1 w .connect v1 v2 w . add_vertex v1 . add_vertex v2
  get_indegrees (AdjGraph g) = 
    M.foldr (\adj acc ->
                foldr (\v acc'-> M.adjust (+1) v acc') acc $ M.keys adj
            ) (M.map (const 0) g) g


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
    putStrLn $ show $ breadth_first ugraph 2
    putStrLn $ show $ get_indegrees dgraph
    putStrLn $ show $ topological_sort dgraph
