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
--, collate
--, collates
--, derelativise
--, derelativises
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


compile m = (rs', ds')
  where
    (rs, ds) = compile' [] m
    rs' = Map.map reverse rs
    ds' = map (\(n,e) -> (reverse n, reslot reverse e)) ds

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
    
    
    
    



{-
flatten prefix dict g@(DefGroup _ defs subs) = 
  where
    dict' = foldl (\d (n, _) -> insert n (CDef n : prefix) d) dict defs
    lookup name = case dict' !? name of
                    Just ref -> ref
                    Nothing -> lookup's prefix subs name
    lookup's prefix 
    lookup' prefix (DefGroup proms defs subs) name = 
-}


{-
compile :: Aliasable e => Module r e -> [([CID r], e)]
compilec prefix ctx@(Context mod dgs) 
compilem prefix (Module 
compileg prefix (DefGroup 
-}

{-
compile :: Aliaable e => Module r e -> [([CID r], e)]
compile m = toList $ compilem empty [] m

compilem d p mod@(Module name exs ims body) = 
  where
    p' = CLib name : p

    subs d = foldl subs' d ims
    subs' d m = compilem d (CLib r : p')

    defs d = foldl defs' d body
    defs' d g = compileg d p' (Context mod []) g

    exps d = foldl exs' d exs
    exps' d (a, b) = insert d (CDef a : p) (lookup b (Context mod [])

    
    d' = foldl (\dz (r, m) -> compilem dz (CLib r : p) m) d ims
    d'' = foldl (\dz g -> compileg dz (CGroup (map snd exs) : p) (Context mod []) g) d' body
    d''' = foldl (\dz (a, b) -> insert dz (CDef a : p) (lookup b (Context mod []))) d'' exs
-}


    
    








{-
data Scope a = Scope { children :: [Scope a]
                     , defs :: [(ID, a)]
                     , promoted :: [ID]
                     } deriving (Show)
-}

{-
type ID = String
data Def a = Def ID a deriving Show
data DefGroup a = DefGroup [Def a] (Scope a) deriving Show
data Scope a = Scope [DefGroup a] deriving Show

data Mod a = Mod { imports :: [Mod a]
                 , body :: Scope a
                 , exports :: [ID]
                 } deriving (Show)
-}

-------

{-
  a weak 'zipper' to keep a record of how we've
  walked along the scope path...
  
  we aren't modifying the scopes so we don't
  need to maintain a one-hole-context, instead
  we can just push the current scope onto our
  scope list, and use this for locating new
  refs

  because the scope list is FILO, the first
  place to search for defs is the head of the
  list, then !!2, !!3, etc until we've walked
  the entire list; then we can try to deref
  via the module

  for the record, the OHC of a scope is:
  (Scope a,
     Scope a,
     [Def a],
       ID,
     [Def a],
   Scope a)
 -}

{-
data Context a = Context (Mod a) [Scope a]

flatten :: Aliasable a => Mod a -> [([Ref a], a)]






locate :: Context a -> a -> Context a
-}


{-
data Ref a = Local a
           | Remote a (Context a)
           deriving Show
-}

-------

class Aliasable a where
    type Sig a :: *
    type Ref a :: *
    type Reslot a b :: *

    sig :: a -> Sig a
    slots :: a -> [Ref a]
    reslot :: Aliasable (Reslot a b) => (Ref a -> b) -> a -> Reslot a b

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
