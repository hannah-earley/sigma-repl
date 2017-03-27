{-# LANGUAGE UnicodeSyntax #-}
--{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Module2 where

import Module( deälias, reslot, slots, Aliasable)

import Data.Maybe (isNothing, catMaybes)
--import Data.Tuple
import Data.List (group, filter, sort, nub, foldl')
import Data.Map.Strict (Map, union, empty, keys, elems, intersection, mapWithKey, fromList)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Control.Monad

data Group e r = Group { defs :: [(r, e r)]
                       , children :: [Group e r]
                       , promotes :: [(r, r)] -- (x, y) => expose x as y
                       , ismod :: Bool
                       } deriving Show

type Contested r = (r, Int)

data Context e r = Context { symbols :: Map (r, Int) (e (r, Int))
                           , exposed :: Map r (r, Int) }

data TempContextOk e r = TempContext { syms :: Map (r, Int) (e (r, Maybe Int))
                                     , expd :: Map r (r, Int) }

type CtxException = String

type CtxMaybe = Either CtxException

type TempContext e r = CtxMaybe (TempContextOk e r)

type AOS e r = (Aliasable e, Ord r, Show r)

cerr :: String -> CtxMaybe a
cerr = Left . ("Compilation error: " ++)

cecf t = cerr . (++) ("competing " ++ t ++ "(s) for :") . show

compile :: AOS e r => Int -> Group e r -> (Int, TempContext e r)
compile m g = (n, do { c <- ctx
                     ; e <- shadow (promotes g) (expd c)
                     ; e' <- safeFromList (cecf "export") e
                     ; return TempContext { syms = syms c, expd = e' } })
  where
    (n, ctx) = foldr compileInc (succ m, prep m g) $ children g
    es = expd <$> ctx >>= shadow (promotes g) >>= safeFromList (cecf "export")


combine :: (Ord r, Show r) => TempContext e r -> TempContext e r -> TempContext e r
combine a' b' =
  do a <- a'
     b <- b'

     let redefs = keys $ syms a `intersection` syms b
         reexps = keys $ expd a `intersection` expd b

     if | not (null redefs) -> error $ "ERROR! Duplicate symbol(s): " ++ show redefs
          -- note that the above should never happen as nums should be distinct...
        | not (null reexps) -> cecf "import" reexps
        | otherwise -> return $ TempContext { syms = syms a `union` syms b
                                            , expd = expd a `union` expd b }


compileInc :: AOS e r => Group e r -> (Int, TempContext e r) -> (Int, TempContext e r)
compileInc g (n, c) = let (n', c') = compile n g in (n', combine c c')

-- combine' a b = do a' <- a
--                   let reexps = keys $ expd a' `intersection` expd b

-- compileInc g (n, c) = let (n', c') = compile n g
--                       in (n', combine c c')

compileSafe :: AOS e r => TempContextOk e r -> Group e r -> TempContext e r
compileSafe c g = snd $ compileInc g (safeNum c, pure c)

incompile :: AOS e r => Context e r -> Group e r -> CtxMaybe (Context e r)
incompile c g = fromContext c >>= toContext . cap . flip compileSafe g

cap :: AOS e r => TempContext e r -> TempContext e r
cap c = unresolved <$> c >>= \u -> if null u then c else
                                      cerr ("couldn't resolve symbol(s): " ++ show u)

fromContext :: Aliasable e => Context e r -> TempContext e r
fromContext c = return TempContext { syms = M.map (reslot wrapSym) $ symbols c
                                   , expd = exposed c }

wrapSym (s, n) = (s, Just n)
unwrapSym (s, Just n) = (s, n)

toContext :: Aliasable e => TempContext e r -> CtxMaybe (Context e r)
toContext = liftM $ \c -> Context { symbols = M.map (reslot unwrapSym) $ syms c
                                , exposed = expd c }

safeNum :: Aliasable e => TempContextOk e r -> Int
safeNum c = foldl' max 0 $ se c ++ ss c ++ sd c
  where
    se = map snd . elems . expd
    ss = map snd . keys . syms
    sd = catMaybes . map snd . concat . map slots . elems . syms

unresolved :: (Ord r, Aliasable e) => TempContextOk e r -> [r]
unresolved = nub . map fst . filter (isNothing . snd) . concat . map slots . elems . syms


-- prep :: (Aliasable e, Ord r) => Int -> Group e r -> TempContext e r
-- prep n = Right . flip TempContext empty . fromList . map go . defs
--   where go (s, d) = ((s, n), reslot (\r -> (r, Nothing)) d)

prep :: AOS e r => Int -> Group e r -> TempContext e r
prep n g = do ss <- safeFromList (cecf "definition") . map go $ defs g
              let es = fromList . map (\s -> (fst s, s)) $ keys ss
              return $ TempContext ss es
  where
    go (s, d) = ((s, n), reslot (\r -> (r, Nothing)) d)

resolutionMap :: Ord r => Map r (r, Int) -> (r, Maybe Int) -> (r, Maybe Int)
resolutionMap e l@(s, Nothing) = maybe l wrapSym (M.lookup s e)
resolutionMap _ l = l

resolve :: (Aliasable e, Ord r) => TempContext e r -> TempContext e r
resolve c' = [c { syms = M.map (reslot . resolutionMap $ expd c) (syms c) } | c <- c']

bastards :: Ord a => [a] -> [a]
bastards = map head . filter ((<= 1) . length) . group . sort

safeFromList :: Ord k => ([k] -> CtxMaybe (Map k v)) -> [(k,v)] -> CtxMaybe (Map k v)
safeFromList errf kvs = let zs = bastards $ map fst kvs
                        in case zs of
                             [] -> Right $ fromList kvs
                             _  -> errf zs

shadow :: (Show r, Ord r) => [(r,r)] -> Map r (r, Int) -> CtxMaybe [(r, (r, Int))]
shadow [] _         = Right []
shadow ((x,y):rs) m = case M.lookup x m of
                        Just z  -> shadow rs m >>= return . (:) (y,z)
                        Nothing -> cerr $ "Can't find symbol to export: " ++ show x