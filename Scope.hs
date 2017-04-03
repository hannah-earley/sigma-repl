{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module Scope
( Resolver(..)
, stack
, wrap
, Scopy(..)
, DefList(..)
, ScopeGroup(..)
, Private(..)
, Insulated(..)
) where

import Data.Maybe (fromMaybe)
import qualified Data.Map.Lazy as M

---

data Resolver r d = Resolver { runResolve :: r -> [(d, Resolver r d)] }

stack :: (Ord r, Scope s) => s r d -> Resolver r d -> Resolver r d
stack s c = Resolver $ \r -> case resolve (stack s c) s r of
                               [] -> runResolve c r
                               ds -> ds

wrap :: (Ord r, Scope s) => s r d -> Resolver r d
wrap s = Resolver $ get s

class Scope a where
  exposed :: a r d -> [r]
  resolve :: Ord r => Resolver r d -> a r d -> r -> [(d, Resolver r d)]

get :: (Scope a, Ord r) => a r d -> r -> [(d, Resolver r d)]
get = resolve (Resolver $ const [])

data Scopy r d = forall s. Scope s => MkScopy (s r d)
instance Scope Scopy where
  exposed (MkScopy s) = exposed s
  resolve c (MkScopy s) = resolve c s

newtype DefList r d = DefList { defs :: M.Map r [d] }
instance Scope DefList where
  exposed l = M.keys . defs $ l
  resolve c l r = map (,stack l c) . fromMaybe [] . M.lookup r . defs $ l

newtype ScopeGroup r d = ScopeGroup { scopes :: [Scopy r d] }
instance Scope ScopeGroup where
  exposed g = concatMap exposed . scopes $ g
  resolve c g r = 
    let f s = resolve (stack g c) s r
    in concatMap f . scopes $ g

data Private r d = forall s. Scope s =>
                   Private { scope :: s r d , public :: [r] }
instance Scope Private where
  exposed = public
  resolve c (Private {scope, public}) r
    | r `elem` public = resolve c scope r
    | otherwise       = []

-- one-way mirror, defs inside can't refer to defs outside
data Insulated r d = forall s. Scope s => Insulated (s r d)
instance Scope Insulated where
  exposed (Insulated s) = exposed s
  resolve _ (Insulated s) = get s

{--- tests

dl :: Ord r => [(r,d)] -> Scopy r d
dl = MkScopy . DefList . M.fromListWith (++) . map (pure <$>)

ws = dl [(1,"a"),(2,"b"),(3,"c"),(4,"e")]
xs = dl [(4,"d"),(5,"e"),(6,"f")]
ys = dl [(7,"g"),(8,"h"),(9,"i")]
zs = dl [(1,"j"),(2,"k"),(3,"l")]

g = ScopeGroup [ws,zs]
h = ScopeGroup [xs,ys]
i = ScopeGroup $ [MkScopy h, ws, j, MkScopy p]

p = Private (dl [(1,"p"),(10,"q"), (11, "r"), (12,"z")]) [1,11]

j = MkScopy $ Insulated zs

---}