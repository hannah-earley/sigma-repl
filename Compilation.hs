{-# LANGUAGE TypeFamilies #-}

module Compilation
( module Compilation
) where

import Numeric.Natural (Natural)
import Numeric (showIntAtBase)
import Data.Char (ord, intToDigit)
import qualified Model as Mo
import qualified Scope as S
import qualified Common as C
import qualified Data.Map.Lazy as Ma
import Data.Function (on)
import Control.Monad (foldM, join, guard)
import Data.List (nub)

--- parsing intermediate

data Expr = Seq [Expr]
          | Perm [Expr] [Expr]
          | As String Expr
          | Ref String
          | Label String
          deriving Show

--- helper functions

r :: String -> Expr
r = Ref

l :: String -> Expr
l = Label

stop :: Expr
stop = Seq []

dat :: [Expr] -> Expr
dat = Seq . ([stop] ++) . (++ [stop])

nil :: Expr
nil = dat[ r"1:", dat[ stop, stop ] ]

pair :: Expr -> Expr -> Expr
pair x y = dat[ r"2:", dat[ x, y ] ]

imlist :: [Expr] -> Expr -> Expr
imlist [] (Seq []) = nil
imlist [x] t = pair x t
imlist (x:xs) t = pair x $ imlist xs t

list :: [Expr] -> Expr
list = flip imlist stop

nat :: Natural -> Expr
nat 0 = dat [ r"1:", dat[ stop, stop ] ]
nat n = dat [ r"2:", nat $ pred n ]

char :: Char -> Expr
char = dat . map (r . (:":")) . lpad 7 '1' . basify . ord
  where basify = ($"") . showIntAtBase 8 (intToDigit . succ)

lpad :: Int -> a -> [a] -> [a]
lpad n x xs = replicate (n - length xs) x ++ xs

sym :: String -> Expr
sym = list . map char

--- choice perms

choice :: Natural -> Expr
choice 0 = Perm [l"fs", l"z", l"g"] [l"g", l"fs", l"z"]
choice n = Perm [fs"f", l"z", l"g"] [l"f", fs"g", l"z"]
  where
    fs = flip imlist (l "fs") . fs'
    fs' l' = map (l . show) [1..n-1] ++ [l l']

choices :: [(String, Expr)]
choices = map (\n -> (shows n ":", choice n)) [0..8]

--- remove top level labels and expand perm labels to avoid
--- cycles and inconsistencies

flatten :: Expr -> Maybe Expr
flatten (Seq xs) = Seq <$> mapM flatten xs
flatten (Perm l' r') = do l'' <- mapM flatten l'
                          r'' <- mapM flatten r'
                          m <- foldM build Ma.empty $ l'' ++ r''
                          guard . C.acyclicp $ depgraph m
                          return $ flatten' m l'' r''
flatten (As _ e) = flatten e
flatten (Ref r') = Just $ Ref r'
flatten (Label _) = Nothing

build :: (m ~ Ma.Map String Expr) => m -> Expr -> Maybe m
build m (Seq xs) = foldM build m xs
build m (As l' e) =
  case Ma.lookup l' m of
    Just _ -> Nothing -- duplicate definition
    Nothing -> build (Ma.insert l' e m) e
build m _ = Just m

depgraph :: Ma.Map String Expr -> [(String, String, [String])]
depgraph = map (uncurry $ join (,,)) . Ma.toList . Ma.map (nub . go)
  where
    go (Seq xs) = concatMap go xs
    go (As l' _) = [l']
    go (Label l') = [l']
    go _ = []

flatten' :: Ma.Map String Expr -> [Expr] -> [Expr] -> Expr
flatten' m = Perm `on` map go
  where
    go (Seq xs) = Seq $ map go xs
    go (As l' e) = maybe e go $ Ma.lookup l' m
    go (Label l') = go $ As l' (Label l')
    go x = x

--- compilation

{-data Expr = Seq [Expr]
          | Perm [Expr] [Expr]
          | As String Expr
          | Ref String
          | Label String
          deriving Show-}

{-
need:
 - implementation for flatten, which should check consistency of
   @s within perms and substitute the finite reductions
 - all these functions should be either'd and return an exception
   on error
 - the compile functions should take:
     - an integer for local perm counting
     - a Map Int Hash for local perm ids
     - a Scope String M.Expr for derefererencing
 - they should also return a list of newly defined local perms
-}

data CompileException = LabelCycle
                      | DataCycle
                      | UndefinedReference

type CompContext = (Int, Ma.Map Int Mo.Hash, S.Scope Mo.ID Mo.Expr)
type Compiled a = Either CompileException (Int, a, [Mo.Perm])

compile :: Expr -> Either CompileException Mo.Expr
--compile :: CompContext -> Expr -> Compiled Mo.Expr
compile e = case flatten e of
              Nothing -> Left LabelCycle
              Just e' -> Right $ compilex e'

compilex :: Expr -> Mo.Expr
--compilex :: CompContext -> Expr -> Compiled Mo.Expr
compilex (Seq xs) = Mo.ESeq Mo.Anon $ map compilex xs
compilex (Perm l' r') = Mo.EPerm . Mo.Local $ compilep l' r'
compilex (Ref _) = undefined
compilex _ = undefined

compilepx :: Expr -> Mo.PExpr
--compilepx :: CompContext -> Expr -> Compiled Mo.PExpr
compilepx (Seq xs) = Mo.PSeq Mo.Anon $ map compilepx xs
compilepx (As l' (Seq xs)) = Mo.PSeq (Mo.ByLabel l') $ map compilepx xs
compilepx (Perm l' r') = Mo.PPerm . Mo.Local $ compilep l' r'
compilepx (Label l') = Mo.PLabel l'
compilepx (Ref _) = undefined
compilepx _ = undefined

compilep :: [Expr] -> [Expr] -> Mo.Perm
--compilep :: CompContext -> [Expr] -> [Expr] -> Compiled Mo.Perm
compilep = Mo.Perm (Mo.PID 0 0) `on` map compilepx
