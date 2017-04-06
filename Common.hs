module Common (module Common) where

import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import qualified Data.Graph as G
import Control.Monad (liftM2)
import Data.Foldable (foldl')

--- Partitioning

type Partition a = S.Set (S.Set a)

pmap :: (Ord a, Ord b) => (a -> b) -> Partition a -> Partition b
pmap = S.map . S.map

plists :: Partition a -> [[a]]
plists = map S.toList . S.toList

partition :: (Ord a, Ord b) => (a -> b) -> S.Set a -> Partition a
partition f = S.fromList . map S.fromList . M.elems . M.fromListWith (++)
                         . map (liftM2 (,) f pure) . S.toList

repartition :: (Ord a, Ord b) => (a -> b) -> Partition a -> Partition a
repartition = (foldl' S.union S.empty .) . S.map . partition

index :: Ord a => Partition a -> a -> Int
index = (M.!) . M.fromList . concat . zipWith (map . flip (,)) [0..] . plists

--- Control

converge :: (a -> a -> Bool) -> (a -> a) -> a -> a
converge = (until =<<) . (=<<)

--- Graphs

acyclicp :: Ord k => [(n, k, [k])] -> Bool
acyclicp = all ((<=1) . length . G.flattenSCC) . G.stronglyConnComp

--- Lists

shows' :: Show a => [a] -> String
shows' = unwords . map show

initlast :: [a] -> Maybe ([a],a)
initlast = foldr go Nothing
  where
    go x Nothing = Just ([],x)
    go x (Just (is,l)) = Just (x:is,l)
