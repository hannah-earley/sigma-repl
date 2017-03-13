{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Module
( Scope
, Mod
, Aliasable(..)
, deälias
, ID
) where

import Data.Maybe
import Data.Tuple
import Data.List
import qualified Data.Map.Strict as Map
(!?) :: Ord k => Map.Map k a -> k -> Maybe a
(!?) = flip Map.lookup

type ID = String

data Scope a = Scope { children :: [Scope a]
                     , defs :: [(ID, a)]
                     , promoted :: [ID]
                     } deriving (Show)

data Mod a = Mod { imports :: [Mod a]
                 , body :: Scope a
                 , exports :: [ID]
                 } deriving (Show)

-------

class Aliasable a where
    type Sig a :: *
    type Ref a :: *
    sig :: a -> Sig a
    slots :: a -> [Ref a]

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

deälias :: (Aliasable a, Ord (Sig a), Ord (Ref a)) => [(Ref a, a)] -> [[Ref a]]
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
