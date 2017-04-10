{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Representation
( module Representation
) where

import Common (Partition, subpartition, index, pmap, converge, partition)
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Control.Monad (ap)
import Data.Hashable (Hashable, hash)

distinguish :: Ord r => Partition (r, [r]) -> Partition (r, [r])
distinguish = subpartition =<< (. snd) . map . (M.!) . index . pmap fst

dedup :: Ord r => Partition (r, [r]) -> Partition r
dedup = pmap fst . converge (==) distinguish

duprep :: (Ord r, Ord s) => [(r, (s, [r]))] -> Partition (r, [r])
duprep = pmap (fmap snd) . partition (fst . snd) . S.fromList

-- in theory, this creates a unique representation of the deäliased subgraphs
-- rooted by each ref in xs; the representation is a list of the unique nodes
-- in the subgraph, with the root first, and each node represented by its
-- signature and the nodes it points to (indexed by their position in the
-- list). The positions should correspond to a breadth-first search, and
-- also inherit order from the reference order. Thus it should provide an
-- efficient test for labelled directed cyclic graph isomorphism....
--
-- graph isomorphism is generally a hard problem though, not known if p or np,
-- so this algorithm may be incorrect; hopefully it is correct as each node's
-- outedges have a unique set of labels
--
-- by doi:10.1007/11751649_46, this should be fine? unique labels
-- should reduce to O(n) for n nodes, and so our special case algorithm
-- should work well....
uniqify :: (Ord r, Ord s) => [(r, (s, [r]))] -> [(r, [(s, [Int])])]
uniqify xs = map (ap (,) bfs . fst) xs
  where
    -- alias and original lookup tables
    alut = (M.!) . index . dedup . duprep $ xs
    xlut = (M.!) . M.fromList $ xs

    -- breadth first search to construct a uniquely ordered
    -- representation of the dependency graph
    bfs root = let (m, g) = go (m M.!) (M.empty, []) root in g
    go f (m,g) r = if alut r `M.member` m
                   then (m,g)
                   else let m' = M.insert (alut r) (M.size m) m
                            (sig, refs) = xlut r
                            (m'', g') = foldl (go f) (m', g) refs
                            refs' = map (f . alut) refs
                        in (m'', (sig, refs') : g')

hashify :: (Hashable s, Ord r, Ord s) => [(r, (s, [r]))] -> [(r, Int)]
hashify = map (fmap hash) . uniqify
