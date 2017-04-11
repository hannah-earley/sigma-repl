module Core
( module Core
) where

import Numeric.Natural (Natural)
import Numeric (showIntAtBase)
import Data.Char (ord, intToDigit)
import qualified Model as M
import Data.Function (on)

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

flatten :: Expr -> Expr
flatten = undefined

compilex :: Expr -> M.Expr
compilex (Seq xs) = M.ESeq M.Anon $ map compilex xs
compilex (Perm l' r') = M.EPerm . M.Local $ compilep l' r'
compilex (Ref _) = undefined
compilex _ = undefined

compilepx :: Expr -> M.PExpr
compilepx (Seq xs) = M.PSeq M.Anon $ map compilepx xs
compilepx (As l' (Seq xs)) = M.PSeq (M.ByLabel l') $ map compilepx xs
compilepx (Perm l' r') = M.PPerm . M.Local $ compilep l' r'
compilepx (Label l') = M.PLabel l'
compilepx (Ref _) = undefined
compilepx _ = undefined

compilep :: [Expr] -> [Expr] -> M.Perm
compilep = M.Perm (M.PID 0 0) `on` map compilepx
