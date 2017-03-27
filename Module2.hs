{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Module2
( compile
, incompile
, contextualise
, Context(..)
, CtxException
) where

import Module( deälias, reslot, slots, Aliasable)
import Data.Maybe (isNothing, catMaybes)
import Data.List (group, filter, sort, nub, foldl')
import Data.Map.Strict (Map, union, keys, elems, intersection, fromList)
import qualified Data.Map.Strict as M

--- type definitions

data Group e r = Group { defs :: [(r, e r)]
                       , children :: [Group e r]
                       , promotes :: [(r, r)] -- (x, y) => expose x as y
                       , ismod :: Bool
                       } deriving Show

data Context e r = Context { symbols :: Map (r, Int) (e (r, Int))
                           , exposed :: Map r (r, Int) }

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
prep n g = safeFromList (cecf "definition") (map (wrapDef n) $ defs g) >>=
             \ss -> return (n + 1, CtxTemp ss $ expSyms ss)

reexport :: (Ord r, Show r) => [(r,r)] -> (Int, CtxTemp e r) -> TempContext e r
reexport ps (n, c) = shadow ps (expd c) >>= safeFromList (cecf "export")
                                        >>= \e -> return (n, c { expd = e })

resolutionMap :: Ord r => Map r (r, Int) -> (r, Maybe Int) -> (r, Maybe Int)
resolutionMap e l@(s, Nothing) = maybe l wrapSym (M.lookup s e)
resolutionMap _ l = l

resolve :: (Aliasable e, Ord r) => CtxTemp e r -> CtxTemp e r
resolve c = c { syms = M.map (reslot . resolutionMap $ expd c) (syms c) }

unresolved :: (Ord r, Aliasable e) => CtxTemp e r -> [r]
unresolved = notfound . concat . map slots . elems . syms

cap :: AOS e r => (Int, CtxTemp e r) -> TempContext e r
cap (n, c) = (n,) <$> let u = unresolved c
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
cecf t = cerr . (++) ("competing " ++ t ++ "(s) for :") . show

expSyms = fromList . map (\s -> (fst s, s)) . keys
wrapDef n (s, d) = ((s, n), reslot (\r -> (r, Nothing)) d)
wrapSym (s, n) = (s, Just n)
unwrapSym (s, Just n) = (s, n)

excesses :: Ord a => [a] -> [a]
excesses = map head . filter ((<= 1) . length) . group . sort

safeFromList :: Ord k => ([k] -> CtxMaybe (Map k v)) -> [(k,v)] -> CtxMaybe (Map k v)
safeFromList errf kvs = let zs = excesses $ map fst kvs
                        in case zs of
                             [] -> Right $ fromList kvs
                             _  -> errf zs

notfound = nub . map fst . filter (isNothing . snd)