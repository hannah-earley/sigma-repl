{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module Scope
( Context(..)
, Scopy(..)
, DefList(..)
, ScopeGroup(..)
, Restricted(..)
, Insulated(..)
, Scope(..)
) where

import qualified Data.Map.Lazy as Map

---

class Scope a where
  exposed :: a r d -> [r]
  resolve :: Ord r => Context r d -> a r d -> r -> [(d, Context r d)]
  get :: Ord r => a r d -> r -> [(d, Context r d)]
  get = resolve $ Context { ctxExposed = [], ctxResolve = const [] }

data Context r d = Context { ctxExposed :: [r]
                           , ctxResolve :: r -> [(d, Context r d)] }
instance Scope Context where
  exposed = ctxExposed
  resolve _ = ctxResolve
  get = ctxResolve

stack :: (Ord r, Scope s) => s r d -> Context r d -> Context r d
stack s c = Context { ctxExposed = exposed s ++ exposed c
                    , ctxResolve = \r -> case resolve c s r of
                                           [] -> ctxResolve c r
                                           ds -> ds }

data Scopy r d = forall s. Scope s => MkScopy (s r d)
instance Scope Scopy where
  exposed (MkScopy s) = exposed s
  resolve c (MkScopy s) = resolve c s

newtype DefList r d = DefList { defs :: Map.Map r [d] }
instance Scope DefList where
  exposed l = Map.keys . defs $ l
  resolve c l r = maybe [] (map (,stack l c)) . Map.lookup r . defs $ l

newtype ScopeGroup r d = ScopeGroup { scopes :: [Scopy r d] }
instance Scope ScopeGroup where
  exposed g = concatMap exposed . scopes $ g
  resolve c g r = 
    let f s = resolve (stack g c) s r
    in concatMap f . scopes $ g

data Restricted r d = forall s. Scope s =>
                      Restricted { scope :: s r d
                                 , public :: Map.Map r r }
instance Scope Restricted where
  exposed = Map.keys . public
  resolve c (Restricted {scope, public})
    = maybe [] (resolve c scope) . flip Map.lookup public

-- one-way mirror, defs inside can't refer to defs outside
data Insulated r d = forall s. Scope s => Insulated (s r d)
instance Scope Insulated where
  exposed (Insulated s) = exposed s
  resolve _ (Insulated s) = get s

{--- tests

instance Show r => Show (Context r d) where
  show = show . ctxExposed

dl :: Ord r => [(r,d)] -> Scopy r d
dl = MkScopy . DefList . Map.fromListWith (++) . map (pure <$>)

ws = dl [(1,"a"),(2,"b"),(3,"c"),(4,"e")]
xs = dl [(4,"d"),(5,"e"),(6,"f")]
ys = dl [(7,"g"),(8,"h"),(9,"i")]
zs = dl [(1,"j"),(2,"k"),(3,"l")]

g = ScopeGroup [ws,zs]
h = ScopeGroup [xs,ys]
i = ScopeGroup $ [MkScopy h, ws, j, MkScopy p, MkScopy q]

private :: (Scope s, Ord r) => s r d -> [r] -> Restricted r d
private s rs = Restricted s (Map.fromList $ zip rs rs)

p = private (dl [(1,"p"),(10,"q"), (11, "r"), (12,"z")]) [1,11]
q = Restricted (dl [(1,"m"),(2,"n"),(3,"o")]) $ Map.fromList [(13,1),(14,2),(7,3)]

j = MkScopy $ Insulated zs

go :: Ord r => Int -> [(d, Context r d)] -> r -> [(d, Context r d)]
go n = get . snd . (!!n)

---}