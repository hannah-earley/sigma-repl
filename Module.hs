{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Module
( Aliasable(..)
, deälias

, Group(..)
, CID(..)
, compile
) where

import Data.Maybe
import Data.Tuple
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

---

(!?) :: Ord k => Map.Map k a -> k -> Maybe a
(!?) = flip Map.lookup
mapmap :: (Ord k, Ord k') => (k -> k') -> (v -> v') -> Map.Map k v -> Map.Map k' v'
mapmap fk fv = Map.fromList . map (\(k, v) -> (fk k, fv v)) . Map.toList

---

data CID r = CLabel r
           | CGroup Int
           | CUpper
           | CError
           deriving (Eq, Ord, Show)

data Group e r = Group { defs :: [(r, e r)]
                       , children :: [Group e r]
                       , promotes :: [(r, r)] -- (x, y) => expose x as y
                       , sandbox :: Bool
                       } deriving Show

---

type CRef r = [CID r]
type CExpr e r = e (CRef r)
type CMap e r = Map.Map (CRef r) (CExpr e r)

compile :: (Ord r, Aliasable e) => Group e r -> CMap e r
compile = collate . prep

collate :: (Ord r, Aliasable e) => Group e (CRef r) -> CMap e r
collate g = strip g . combine . subcollate $ g

---

subcollate :: (Ord r, Aliasable e) => Group e (CRef r) -> [CMap e r]
subcollate g = foldr (\c l -> collate c : l) [entries] $ children g
  where entries = foldl promote (Map.fromList $ defs g) $ promotes g

combine :: (Ord r, Aliasable e) => [CMap e r] -> CMap e r
combine = foldl go Map.empty . zip [1,3..]
  where go l (n, r) = let r' = prefixes (CGroup n) r
                          cols = Map.keys l `intersect` Map.keys r'
                          l' = foldl (demote (CGroup (n+1) :)) l cols
                      in Map.union r' l'

strip :: (Ord r, Aliasable e) => Group e (CRef r) -> CMap e r -> CMap e r
strip g scope = if sandbox g then boxed else stripped
  where
    exps = Set.fromList . exposed $ Map.keys scope
    prms = Set.fromList . map snd $ promotes g
    refs = Set.fromList . exposed . concat . map slots $ Map.elems scope

    up = map (CUpper :) . Set.toList
    precocious = up $ exps Set.\\ prms
    unresolved = up $ refs Set.\\ (exps `Set.intersection` prms)

    stripped = foldl (demote (CGroup 0 :)) scope precocious
    boxed = foldl (demote (CError :)) stripped unresolved

---

prep :: (Ord r, Aliasable e) => Group e r -> Group e (CRef r)
prep (Group ds cs ps sb) = Group { defs = map (\(n,d) -> (wrap n, reslot wrap d)) ds
                                 , children = map prep cs
                                 , promotes = map (\(m,n) -> (wrap m, wrap n)) ps
                                 , sandbox = sb }
                           where wrap r = [CLabel r]

prefixes :: (Ord r, Aliasable e) => CID r -> CMap e r -> CMap e r
prefixes p = mapmap (prefix p) (reslot $ prefix p)

prefix :: CID r -> CRef r -> CRef r
prefix _ x@(CUpper : _) = x
prefix p x = p : x

demote :: (Ord r, Aliasable e) => (CRef r -> CRef r) -> CMap e r -> CRef r -> CMap e r
demote pf m x@(CUpper : y) = mapmap replace' (reslot replace') m
  where replace' = replace x (pf y)
demote _ m _ = m

promote :: (Ord r, Aliasable e) => CMap e r -> (CRef r, CRef r) -> CMap e r
promote m (x, y) = mapmap replace' (reslot replace') m
  where replace' = replace x (CUpper : y)

replace :: Eq a => a -> a -> a -> a
replace old new val
  | val == old = new
  | otherwise  = val

exposed :: [CRef r] -> [CRef r]
exposed = catMaybes . map p
  where
    p (CUpper : x) = Just x
    p _ = Nothing

-------

class Aliasable a where
    type Sig a :: *
    sig :: Ord b => a b -> Sig a
    slots :: Ord b => a b -> [b]
    reslot :: (r -> s) -> a r -> a s

resolve :: Ord r => [([r], s)] -> [([r], Maybe [s])]
resolve defs = map resolve' defs
  where
    m = Map.fromList $ map (\(l:_, s) -> (l, s)) defs
    resolve' (rs, s) = (rs, sequence $ map (m !?) rs)

segregate :: Ord ss => [(rs, Maybe ss)] -> [[rs]]
segregate rs = map snd ok ++ (concat $ map (map (:[]) . snd) error)
  where
    grouped = Map.toList . Map.fromListWith (++) $ map (\(x,y) -> (y,[x])) rs
    (ok, error) = partition (isJust . fst) grouped

regroup :: [[[r]]] -> [([r], Int)]
regroup = concat . zipWith (\n -> map (,n)) [1..]

aliases :: Ord r => [[[r]]] -> [[r]]
aliases = sort . map sort . map (map head)

--deälias :: (Aliasable a, Ord (Sig a), Ord (Ref a)) => [(Ref a, a)] -> [[Ref a]]
deälias :: (Aliasable a, Ord (Sig a), Ord r) => [(r, a r)] -> [[r]]
deälias = go . iterate iter . prep
  where
    prep = segregate . map (\(l, f) -> (l : slots f, Just $ sig f))
    iter = segregate . resolve . regroup
    go (x:(ys@(y:_)))
      | axs == ays = ays
      | otherwise  = go ys
      where
        axs = aliases x
        ays = aliases y
