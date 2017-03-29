{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Output where

import Numeric.Natural
import Model

-- structural debugging

class Debug a where
  debug' :: Natural -> a -> String
  debug :: a -> String
  debug = debug' 5

instance (Show a, Show b) => Debug (Expr a b) where
  debug' n (Perm xs xs') = "<pi " ++ debugs (n-1) xs ++ " | " ++ debugs (n-1) xs' ++ " pi>"
  debug' n (Seq xs) = "(" ++ debugs (n-1) xs ++ ")"
  debug' n (As l x) = ('$' : show l) ++ ('@' : debug' (n-1) x)
  debug' _ (Label l) = '$' : show l
  debug' _ (Ref r) = show r
  debug' _ Stop = "_"

debugs :: (Show a, Show b) => Natural -> [Expr a b] -> String
debugs n = unwords . map (debug' n)


-- structural visualisation

instance (Show a, Show b, Eq a, Eq b) => Show (Expr a b) where
  show (Perm xs xs') = "<pi " ++ prints xs ++ " | " ++ prints xs' ++ " pi>"
  show (Seq xs) = case datap xs of
                    Left d -> "{" ++ prints d ++ "}"
                    -- Left d -> printd d
                    Right s -> "(" ++ prints s ++ ")"
  show (As l x) = ('$' : show l) ++ ('@' : show x)
  show (Label l) = '$' : show l
  show (Ref r) = show r
  show Stop = "_"

prints :: (Eq a, Eq b, Show a, Show b) => [Expr a b] -> String
prints = unwords . map show

datap :: (Eq a, Eq b) => [Expr a b] -> Either [Expr a b] [Expr a b]
datap xs'@(Stop:xs@(_:_))
  | last xs == Stop = Left $ init xs
  | otherwise = Right xs'
datap xs = Right xs

-- special printing sugar for datatypes...
-- printd :: [Expr String] -> String

--   -- lists
-- printd [Ref "nil", Stop] = "[]"
-- printd [Ref "cons", Seq [Stop, x, y, Stop]] = "[" ++ (unwords $ show x : printl y) ++ "]"
--   where
--     printl :: Expr String -> [String]
--     printl (Seq [Stop, Ref "cons", Seq [Stop, x, y, Stop], Stop]) = show x : printl y
--     printl (Seq [Stop, Ref "nil", Stop, Stop]) = []
--     printl x = [". " ++ show x]

--   -- numbers
-- printd [Ref "zero", Stop] = "#0"
-- printd [Ref "succ", n] = printn 1 n
--   where
--     printn :: Natural -> Expr String -> String
--     printn m (Seq [Stop, Ref "zero", Stop, Stop]) = '#' : show m
--     printn m (Seq [Stop, Ref "succ", n, Stop]) = printn (m+1) n
--     printn m x = "{#" ++ show m ++ " . " ++ show x ++ "}"

--   -- fallback
-- printd xs = "{" ++ prints xs ++ "}"
