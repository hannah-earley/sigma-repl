{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Module
( Aliasable(..)
, deälias
, compile
, incompile
, contextualise
, Context(..)
, CtxException
, Group(..)
) where

import Data.Maybe (isNothing, isJust, catMaybes)
import Data.List (group, filter, sort, nub, foldl', partition)
import Data.Map.Strict (Map, union, keys, elems, intersection, fromList)
import qualified Data.Map.Strict as M

(!?) :: Ord k => Map k a -> k -> Maybe a
(!?) = flip M.lookup

--- deäliasing machinery

class Aliasable a where
    type Sig a :: *
    sig :: a b -> Sig a
    slots :: a b -> [b]
    reslot :: (r -> s) -> a r -> a s

resig :: Ord r => [([r], s)] -> [([r], Maybe [s])]
resig defs = map resig' defs
  where
    m = fromList $ map (\(l:_, s) -> (l, s)) defs
    resig' (rs, _) = (rs, sequence $ map (m !?) rs)

segregate :: Ord ss => [(rs, Maybe ss)] -> [[rs]]
segregate rs = map snd ok ++ (concat $ map (map (:[]) . snd) error)
  where
    grouped = M.toList . M.fromListWith (++) $ map (\(x,y) -> (y,[x])) rs
    (ok, error) = partition (isJust . fst) grouped

regroup :: [[[r]]] -> [([r], Int)]
regroup = concat . zipWith (\n -> map (,n)) [1..]

aliases :: Ord r => [[[r]]] -> [[r]]
aliases = sort . map sort . map (map head)

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys

deälias :: (Aliasable a, Ord (Sig a), Ord r) => [(r, a r)] -> [[r]]
deälias = converge (==) . map aliases . iterate iter . prep
  where
    prep = segregate . map (\(l, f) -> (l : slots f, Just $ sig f))
    iter = segregate . resig . regroup

--- type definitions

data Group e r = Group { defs :: [(r, e r)]
                       , children :: [Group e r]
                       , promotes :: [(r, r)] -- (x, y) => expose x as y
                       , ismod :: Bool
                       } deriving Show

data Context e r = Context { symbols :: Map (r, Int) (e (r, Int))
                           , exposed :: Map r (r, Int) }

deriving instance (Show r, Show (e (r, Int))) => Show (Context e r)

data CtxTemp e r = CtxTemp { syms :: Map (r, Int) (e (r, Maybe Int))
                           , expd :: Map r (r, Int) }

type CtxException = String

type CtxMaybe = Either CtxException

type TempContext e r = CtxMaybe (Int, CtxTemp e r)

type AOS e r = (Aliasable e, Ord r, Show r)

--- public functions

compile :: AOS e r => Group e r -> CtxMaybe (Context e r)
compile g = toContext . snd <$> compile' 0 g

incompile :: AOS e r => Context e r -> Group e r -> CtxMaybe (Context e r)
incompile c g = fromContext c >>= collate g >>= cap >>= return . toContext . snd

contextualise :: AOS e r => Context e r -> e r -> CtxMaybe (e (r, Int))
contextualise c e = let e' = reslot (resolutionMap (exposed c) . (,Nothing)) e
                    in if Nothing `elem` (map snd $ slots e')
                       then err . show . notfound $ slots e'
                       else Right $ reslot unwrapSym e'
  where err = Left . (++) "Evaluation error: couldn't resolve symbol(s): "

--- conversion functions

fromContext :: Aliasable e => Context e r -> TempContext e r
fromContext c = let c' = CtxTemp { syms = M.map (reslot wrapSym) $ symbols c
                                 , expd = exposed c }
                in return (safeNum c', c')

toContext :: Aliasable e => CtxTemp e r -> Context e r
toContext c = Context { symbols = M.map (reslot unwrapSym) $ syms c
                      , exposed = expd c }

safeNum :: Aliasable e => CtxTemp e r -> Int
safeNum c = succ . foldl' max 0 $ se c ++ ss c ++ sd c
  where
    se = map snd . elems . expd
    ss = map snd . keys . syms
    sd = catMaybes . map snd . concat . map slots . elems . syms

--- main compilation logic

compile' :: AOS e r => Int -> Group e r -> TempContext e r
compile' n g = foldr f (prep n g) (children g) >>= res
                >>= reexport (promotes g) >>= shine
  where
    f x z = z >>= collate x
    shine = if ismod g then cap else return
    res (n, c) = return (n, resolve c)

collate :: AOS e r => Group e r -> (Int, CtxTemp e r) -> TempContext e r
collate g (n, c) = compile' n g >>= combine c

combine :: (Ord r, Show r) => CtxTemp e r -> (Int, CtxTemp e r) -> TempContext e r
combine c (n', c') = let reexps = keys $ expd c `intersection` expd c'
                     in if not $ null reexps then cecf "import" reexps
                        else return (n', CtxTemp (syms c `union` syms c')
                                                 (expd c `union` expd c'))

--- compilation helper functions

prep :: AOS e r => Int -> Group e r -> TempContext e r
prep n g = safeFromList (cecf "definition" . map fst) (map (wrapDef n) $ defs g)
             >>= \ss -> return (n + 1, CtxTemp ss $ expSyms ss)

reexport :: (Ord r, Show r) => [(r,r)] -> (Int, CtxTemp e r) -> TempContext e r
reexport ps (n, c) = shadow ps (expd c) >>= safeFromList (cecf "export")
                                        >>= \e -> return (n, c { expd = e })

resolutionMap :: Ord r => Map r (r, Int) -> (r, Maybe Int) -> (r, Maybe Int)
resolutionMap e l@(s, Nothing) = maybe l wrapSym (M.lookup s e)
resolutionMap _ l = l

resolve :: (Aliasable e, Ord r) => CtxTemp e r -> CtxTemp e r
resolve c = c { syms = M.map (reslot . resolutionMap $ expd c) (syms c) }

cap :: AOS e r => (Int, CtxTemp e r) -> TempContext e r
cap (n, c) = (n,) <$> let u = notfound . concat . map slots . elems $ syms c
                      in if null u then return c else cerr $ msg ++ show u
  where msg = "couldn't resolve symbol(s): "

shadow :: (Show r, Ord r) => [(r,r)] -> Map r (r, Int) -> CtxMaybe [(r, (r, Int))]
shadow [] _         = Right []
shadow ((x,y):rs) m = case M.lookup x m of
                        Just z  -> shadow rs m >>= return . (:) (y,z)
                        Nothing -> cerr $ "Can't find symbol to export: " ++ show x

--- exceptional and miscellaneous helper functions

cerr :: String -> CtxMaybe a
cerr = Left . ("Compilation error: " ++)

cecf :: Show t => String -> t -> CtxMaybe a
cecf t = cerr . (++) ("competing " ++ t ++ "(s) for: ") . show

expSyms = fromList . map (\s -> (fst s, s)) . keys
wrapDef n (s, d) = ((s, n), reslot (\r -> (r, Nothing)) d)
wrapSym (s, n) = (s, Just n)
unwrapSym (s, Just n) = (s, n)

excesses :: Ord a => [a] -> [a]
excesses = map head . filter ((> 1) . length) . group . sort

safeFromList :: Ord k => ([k] -> CtxMaybe (Map k v)) -> [(k,v)] -> CtxMaybe (Map k v)
safeFromList errf kvs = let zs = excesses $ map fst kvs
                        in case zs of
                             [] -> Right $ fromList kvs
                             _  -> errf zs

notfound :: Ord a => [(a, Maybe b)] -> [a]
notfound = nub . map fst . filter (isNothing . snd)