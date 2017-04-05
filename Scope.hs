{-# LANGUAGE ExistentialQuantification #-}

module Scope (module Scope) where

import Data.List (nub)
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy ( Map, keys, unionsWith, unionWith
                     , toList, fromList, findWithDefault )

type Crumb = String

class Scope a where
  exposed :: Eq r => a r d -> [r]
  resolve :: Ord r => Context r d -> a r d -> r -> [(d, Context r d)]
  get :: Ord r => a r d -> r -> [(d, Context r d)]
  get = resolve empty
  dups :: Ord r => Context r d -> a r d -> Map r [[Crumb]]

-- 
data Context r d =
  Context { ctxTrail :: [Crumb]
          , ctxExposed :: [r]
          , ctxResolve :: r -> [(d, Context r d)]
          , ctxDups :: Map r [[Crumb]] }
instance Scope Context where
  exposed = ctxExposed
  resolve _ = get
  get = ctxResolve
  dups _ = ctxDups

-- type coercion
data Scopy r d = forall s. Scope s => MkScopy (s r d)
instance Scope Scopy where
  exposed (MkScopy s) = exposed s
  resolve c (MkScopy s) = resolve c s
  dups c (MkScopy s) = dups c s

-- scope primitive
newtype DefList r d = DefList { defs :: Map r [d] }
instance Scope DefList where
  exposed l = keys . defs $ l

  resolve c l r = zipWith (flip (,) . stack l c) [1..]
                    . findWithDefault [] r $ defs l

  dups c l = dups' (resolve c l) (exposed l)

-- scope combiner
newtype ScopeGroup r d = ScopeGroup { scopes :: [Scopy r d] }
instance Scope ScopeGroup where
  exposed = concatMap exposed . scopes

  resolve c g r = let f (n,s) = resolve (stack g c n) s r
                  in concatMap f . zip [1..] $ scopes g

  dups c g = let subs = zipWith (dups . stack g c) [1..] (scopes g)
                 collisions = dups' (resolve c g) (exposed g)
             in Map.map nub . unionsWith (++) $ collisions : subs

-- limit exposure and allows symbol renaming
-- note that you can also reëxport a symbol
-- multiple times under multiple names
data Restricted r d = forall s. Scope s =>
  Restricted { scope :: s r d, public :: Map r r }
instance Scope Restricted where
  exposed (Restricted {scope = s, public = p})
    = map fst . filter (flip elem (exposed s) . snd) $ toList p

  resolve c (Restricted {scope = s, public = p})
    = maybe [] (resolve c s) . flip Map.lookup p

  dups c (Restricted {scope = s}) = dups c s

-- one-way mirror, defs inside can't refer to defs outside
data Insulated r d = forall s. Scope s => Insulated (s r d)
instance Scope Insulated where
  exposed (Insulated s) = exposed s
  resolve c (Insulated s) = resolve (empty {ctxTrail = ctxTrail c}) s
  dups c (Insulated s) = dups c s

-- automates (see mkFile) insulating restriction of a given scope,
-- and crucially, allows naming the scope for error messages...
data File r d = File { name :: String, contents :: Insulated r d }
instance Scope File where
  exposed = exposed . contents
  resolve c f = resolve (gretel c $ name f) $ contents f
  dups c f = dups (gretel c $ name f) $ contents f

--- helper functions

gretel :: Context r d -> Crumb -> Context r d
gretel ctx cr = ctx {ctxTrail = cr : ctxTrail ctx}

dups' :: Ord r => (r -> [(a, Context r d)]) -> [r] -> Map.Map r [[Crumb]]
dups' rf xs = let f r = (r, map (reverse . ctxTrail . snd) $ rf r)
              in fromList . filter ((>1) . length . snd) $ map f xs

empty :: Context r d
empty = Context [] [] (const []) Map.empty

stack :: (Ord r, Scope s) => s r d -> Context r d -> Int -> Context r d
stack s c n = Context { ctxTrail = show n : ctxTrail c
                      , ctxExposed = exposed s ++ exposed c
                      , ctxResolve = \r -> case resolve c s r of
                                             [] -> ctxResolve c r
                                             ds -> ds
                      , ctxDups = unionWith (++) (ctxDups c) (dups c s) }

mkFile :: Scope s => String -> Map r r -> s r d -> File r d
mkFile name' public' scope' =
  let r = Restricted { scope = scope' , public = public' }
  in File { name = name', contents = Insulated r }