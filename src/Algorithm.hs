module Algorithm
( module Algorithm
) where

import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import qualified Data.Graph as G
import Control.Monad (liftM2)
import Data.Foldable (foldl')

--- Deduplication

-- this algorithm (dedup) find equivalence classes over objects
-- defined by a structural signature (s) and references to other
-- objects by index (r), iteratively repartitioning the list
-- by any distinguishing factors, starting with signature
-- and then by the equivalence class of referred objects,
-- until convergence

distinguish :: Ord r => Partition (r, [r]) -> Partition (r, [r])
distinguish = subpartition =<< (. snd) . map . (M.!) . index . pmap fst

dedup :: Ord r => Partition (r, [r]) -> Partition r
dedup = pmap fst . converge (==) distinguish

duprep :: (Ord r, Ord s) => [(r, (s, [r]))] -> Partition (r, [r])
duprep = pmap (fmap snd) . partition (fst . snd) . S.fromList

--- Sets

unions :: (Foldable t, Ord a) => t (S.Set a) -> S.Set a
unions = foldl' S.union S.empty

--- Partitioning

type Partition a = S.Set (S.Set a)

pmap :: (Ord a, Ord b) => (a -> b) -> Partition a -> Partition b
pmap = S.map . S.map

plists :: Partition a -> [[a]]
plists = map S.toList . S.toList

partition :: (Ord a, Ord b) => (a -> b) -> S.Set a -> Partition a
partition f = S.fromList . map S.fromList . M.elems . M.fromListWith (++)
                         . map (liftM2 (,) f pure) . S.toList

subpartition :: (Ord a, Ord b) => (a -> b) -> Partition a -> Partition a
subpartition = (unions .) . S.map . partition

repartition :: (Ord a, Ord b) => (a -> b) -> Partition a -> Partition a
repartition = (. unions) . partition

index :: Ord a => Partition a -> M.Map a Int
index = M.fromList . concat . zipWith (map . flip (,)) [0..] . plists

--- Control

converge :: (a -> a -> Bool) -> (a -> a) -> a -> a
converge = (until =<<) . (=<<)

--- Graphs

acyclicp :: Ord k => [(n, k, [k])] -> Bool
acyclicp = all ((<=1) . length . G.flattenSCC) . G.stronglyConnComp
