module Scope2
( resolve
, exposed
, collect
, Context
, defines
, group
, restrict
, protect
, file
) where

import Data.List (nub)
import Data.Maybe (catMaybes)
import qualified Data.Map.Lazy as Map

type Crumb = String
type Context r d = (Scope r d, [Crumb], r, d)
data Scope r d = Scope { discover :: [r] -> [r]
                       , reveal :: [Context r d]
                       , search :: SFn' r d }

instance (Show r) => Show (Scope r d) where
  show = show . exposed

type SFn' r d = r -> RFn' (Context r d)
type RFn' a = Maybe a -> (a -> Maybe a) -> Maybe a

--- scope types

defines :: Ord r => Map.Map r d -> Scope r d
defines ds = s
  where
    s = Scope { discover = (Map.keys ds ++)
              , reveal = map lift $ Map.toList ds
              , search = go $ flip Map.lookup ds }
    go m = \r f g -> maybe f (g . lift . (,) r) $ m r
    lift (r,d) = (s,[],r,d)

group :: Eq r => [Scope r d] -> Maybe (Scope r d)
group ss = if uniquep (exposed t) then Just t else Nothing
  where
    ss' = zipWith hansel [1..] ss
    search' r f g = maybe f (g . stackx t)
                  . just 1 $ map (flip resolve r) ss'
    t = Scope { discover = (concatMap exposed ss ++)
              , reveal = concatMap reveal ss'
              , search = search' }

  -- can look out, but only partially in
restrict :: Ord r => Scope r d -> Map.Map r r -> Scope r d
restrict s m = s { discover = map fst . disc, search = search' }
  where
    disc b = filter ((`elem` discover s b) . snd) $ Map.toList m
    search' r f g = maybe f (\r' -> search s r' f g) $ Map.lookup r m

  -- can look in, but not out
protect :: Scope r d -> Scope r d
protect s = t
  where
    t = s { discover = const (exposed s)
          , search = \r _ _ -> stackx t <$> resolve s r }

file :: Ord r => String -> Map.Map r r -> Scope r d -> Scope r d
file f m s = gretel f . protect $ restrict s m

--- scope accessors

resolve :: Scope r d -> r -> Maybe (Context r d)
resolve s r = search s r Nothing Just

exposed :: Scope r d -> [r]
exposed s = discover s []

collect :: Scope r d -> [Context r d]
collect = reveal

--- helper functions

stack :: Scope r d -> Scope r d -> Scope r d
s `stack` t =
  Scope { discover = discover s . discover t
        , reveal = reveal s ++ reveal t
        , search = \r f g -> search s r (search t r f g) (g . stackx t) }

gretel :: Crumb -> Scope r d -> Scope r d
gretel c s = let wrap (t, cs, r, d) = (t, c:cs, r, d)
             in s { reveal = map wrap $ reveal s
                  , search = \r f g -> fmap wrap $ search s r f g }

hansel :: Show a => a -> Scope r d -> Scope r d
hansel = gretel . show

stackx :: Scope r d -> Context r d -> Context r d
stackx t (s, cs, r, d) = (s `stack` t, cs, r, d)

uniquep :: Eq a => [a] -> Bool
uniquep l = length l == length (nub l)

just1 :: [Maybe a] -> Maybe a
just1 xs = case catMaybes xs of 
             [] -> Nothing
             [x] -> Just x
             _ -> error "error: competing definitions"