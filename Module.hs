{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Module
( Aliasable(..)
, deälias
, ID

, DefGroup(..)
, Module(..)
, Context(..)
, CID(..)
, compile
) where

import Data.Maybe
import Data.Tuple
import Data.List
import qualified Data.Map.Strict as Map
(!?) :: Ord k => Map.Map k a -> k -> Maybe a
(!?) = flip Map.lookup


type ID = String

data DefGroup r e = DefGroup { promotions :: [(r, r)]
                             , definitions :: [(r, e)]
                             , subgroups :: [DefGroup r e]
                             } deriving Show

data Module r e = Module { name :: r
                         , exports :: [(r, r)]
                         , imports :: [Module r e]
                         , body :: [DefGroup r e]
                         } deriving Show

data Context r e = Context (Module r e) [[DefGroup r e]]

data CID r = CLib r
           | CGroup Int
           | CDef r deriving Show

type CMap r = Map.Map r [CID r]

collates :: Ord r => [CID r] -> CMap r -> [DefGroup r e] -> CMap r
collates prefix dict gs = foldl collates' dict (zip [1..] gs)
  where
    collates' d (n, g@(DefGroup proms _ _)) = foldl go d proms
      where
        sd = collate (CGroup n : prefix) d g
        go d' (a, b) = Map.alter (const $ sd !? b) a d'

collate :: Ord r => [CID r] -> CMap r -> DefGroup r e -> CMap r
collate prefix dict (DefGroup _ defs subs) = fromSubs $ fromDefs dict
  where
    fromDefs d = foldl (\d' (n, _) -> Map.insert n (CDef n : prefix) d') d defs
    fromSubs d = collates prefix d subs


--derelativises :: [CID r] -> CMap r -> [DefGroup r e] -> [DefGroup [CID r] e]
derelativises prefix dict gs = map derelativises' (zip [1..] gs)
  where
    derelativises' (n, g) = derelativise (CGroup n : prefix) dict g

--derelativise :: [CID r] -> CMap r -> DefGroup r e -> DefGroup [CID r] e
derelativise prefix dict g@(DefGroup proms defs subs) =
                            DefGroup proms' defs' subs'
  where
    dict' = collate prefix dict g
    lu n = Map.findWithDefault (CDef n : prefix) n dict'
    --proms' = map (\(a, b) -> ([CDef a], lu b)) proms
    proms' = []
    defs' = map (\(n, e) -> (CDef n : prefix, reslot lu e)) defs
    subs' = derelativises prefix dict' subs

flattens = concat . map flatten
flatten g@(DefGroup _ ds gs) = ds ++ flattens gs


-- compile :: Module r e -> (Map r [CID r], [([CID r], e')], Map [CID r] Int)
compile m = (rs', ds', grps'')
  where
    (rs, ds) = compile' [] m
    rs' = Map.map reverse rs
    ds' = map (\(n,e) -> (reverse n, reslot reverse e)) ds

    grps = zip [1..] $ deälias ds'
    grps' = map (\(n, es) -> map (,n) es) grps
    grps'' = Map.fromList $ concat grps'

-- compile' :: -> ([(r, [CID r])], [([CID r], Reslot e [CID r])])
compile' prefix (Module name exps ims body) = (refs', defs')
  where
    prefix' = CLib name : prefix
    (refs, defs) = foldl combine (Map.empty, []) $ map (compile' prefix') ims
    combine (refs, defs) (refs', defs') = (Map.union refs' refs, defs ++ defs')
    dict = collates prefix' refs body
    lu n = Map.findWithDefault (CDef n : prefix') n dict
    defs' = flattens $ derelativises prefix' dict body
    refs' = Map.fromList $ map (\(a,b) -> (a, lu b)) exps



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
