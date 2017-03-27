{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Model
( Expr(..)
, bruijns
) where

import Module
import Data.Maybe
import qualified Data.Map.Strict as Map

data Expr l a = Stop
              | Perm [Expr l a] [Expr l a]
              | Seq [Expr l a]
              | Label l
              | As l (Expr l a)
              | Ref a deriving (Eq, Ord)

----

instance Ord l => Aliasable (Expr l) where
    type Sig (Expr l) = Expr [Int] [Int]

    sig = sig' Map.empty

    slots Stop = []
    slots (Label _) = []
    slots (Perm l r) = slots' l ++ slots' r
    slots (Seq xs) = slots' xs
    slots (As l x) = slots x
    slots (Ref r) = [r]

    reslot _ Stop = Stop
    reslot f (Perm l r) = Perm (map (reslot f) l) (map (reslot f) l)
    reslot f (Seq xs) = Seq $ map (reslot f) xs
    reslot f (As l x) = As l $ reslot f x
    reslot f (Label l) = Label l
    reslot f (Ref r) = Ref $ f r

slots' :: Ord l => [Expr l a] -> [a]
slots' = concat . map slots

sig' :: Ord l => Map.Map l [Int] -> Expr l a -> Expr [Int] [Int]
sig' _ Stop = Stop
sig' _ (Perm l r) = Perm (map (sig' m) l) (map (sig' m) r)
  where m = bruijns [2] (Seq r) $ bruijns [1] (Seq l) Map.empty
sig' m (Seq x) = Seq $ map (sig' m) x
sig' m (Label l) = Label . fromMaybe [] $ Map.lookup l m
sig' m (As l x) = As (fromMaybe [] $ Map.lookup l m) (sig' m x)
sig' _ (Ref _) = Ref []

bruijns :: Ord l => [Int] -> Expr l a -> Map.Map l [Int] -> Map.Map l [Int]
bruijns _ Stop m = m
bruijns _ (Perm _ _) m = m
bruijns pre (Seq xs) m = foldl (\m' (n, x) -> bruijns (n:pre) x m') m $ zip [1..] xs
bruijns pre (Label l) m = Map.alter f l m
  where
    f Nothing = Just pre
    f x@(Just _) = x
bruijns pre (As l x) m = bruijns pre x $ bruijns pre (Label l) m
bruijns _ (Ref _) m = m
