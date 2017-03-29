{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Model
( Expr(..)
, bruijns
) where

import Module
import Data.Maybe
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Hashable (Hashable, hash)

--- definition

data Expr l a = Stop
              | Perm [Expr l a] [Expr l a]
              | Seq [Expr l a]
              | Label l
              | As l (Expr l a)
              | Ref a deriving (Eq, Ord)

--- stringy definitions

type ID = String
type Expression = Expr ID (ID, Int)
type Ctx = Context (Expr ID) ID
type Symbol = (ID, Int)

--- deäliasing machinery

{-signature contexts

Label and As terms are meant to only be found within permutation contexts,
but they can be useful outside of permutations (and it simplifies the data
types to unify these two contexts); However, these terms behave differently
inside and out:

 - inside, labels and as expressions are subject to alpha equivalence
 - outside, labels behave like lisp symbols, and as expressions behave
   as simply associating a label to the expression in question; thus
   symbol-labels are now equivalent iff their values are

We have an ambiguity, do we consider associated expression labels as impotent,
or should we require that if two expressions are identically labelled then
they should be equivalent? The simpler option is to make them impotent.

-}
data SigCtx = SigPerm | SigExpr

pmap f (Perm l r) = Perm (f l) (f r)

instance (Hashable l, Ord l) => Aliasable (Expr l) where
    type Sig (Expr l) = Expr [Int] [Int]

    sig = sig' SigExpr M.empty

    slots (Perm l r) = slots' l ++ slots' r
    slots (Seq xs) = slots' xs
    slots (As l x) = slots x
    slots (Ref r) = [r]
    slots _ = []

    reslot f p@(Perm _ _) = flip pmap p . map $ reslot f
    reslot f (Seq xs) = Seq $ map (reslot f) xs
    reslot f (As l x) = As l $ reslot f x
    reslot f (Ref r) = Ref $ f r
    reslot _ (Label l) = Label l
    reslot _ Stop = Stop

    slotp (Ref r) = Just r
    slotp _ = Nothing

slots' :: (Hashable l, Ord l) => [Expr l a] -> [a]
slots' = concat . map slots

sig' :: (Hashable l, Ord l) => SigCtx -> Map l [Int] -> Expr l a -> Expr [Int] [Int]
sig' _ _ Stop = Stop
sig' _ _ (Ref _) = Ref []
sig' c m (Seq x) = Seq $ map (sig' c m) x

sig' _ _ p@(Perm l r) = flip pmap p . map . sig' SigPerm
                                    . bruijns [2] (Seq r)
                                    . bruijns [1] (Seq l)
                                    $ M.empty

sig' c m (Label l) = case c of
                       SigExpr -> Label [-1, hash l]
                       SigPerm -> Label $ M.findWithDefault [] l m

sig' c m (As l x) = case c of
                      SigExpr -> sig' c m x
                      SigPerm -> As (M.findWithDefault [] l m) $ sig' c m x


bruijns :: Ord l => [Int] -> Expr l a -> Map l [Int] -> Map l [Int]
bruijns pre (Seq xs) m = foldl (\m' (n, x) -> bruijns (n:pre) x m') m $ zip [1..] xs
bruijns pre (Label l) m = M.alter (Just . maybe pre id) l m
bruijns pre (As l x) m = bruijns pre x $ bruijns pre (Label l) m
bruijns _ _ m = m

--- evaluation

data Direction = Up | Down

disambiguate :: Ord r => Context (Expr l) r -> r -> Maybe (r, Int)
disambiguate = flip M.lookup . exposed

fetch :: Ord r => Context (Expr l) r -> (r, Int) -> Maybe (Expr l (r, Int))
fetch = flip M.lookup . symbols

disfetch :: Ord r => Context (Expr l) r -> r -> Maybe (Expr l (r, Int))
disfetch c r = return r >>= disambiguate c >>= fetch c 

terminus :: Ord r => Context (Expr l) r -> Expr l (r, Int) -> Bool
terminus _ (Perm _ _) = False
terminus c (As _ e)   = terminus c e
terminus c (Ref r)    = maybe True (terminus c) $ fetch c r
terminus _ _          = True

terminated :: Ord r => Context (Expr l) r -> Direction -> Expr l (r, Int) -> Bool
terminated c d (As _ e) = terminated c d e
terminated c d (Ref r) = maybe True (terminated c d) $ fetch c r
terminated _ _ (Seq []) = True
terminated c Down (Seq (x : _)) = terminus c x
terminated c Up (Seq s@(_ : _)) = terminus c $ last s
terminated _ _ _ = True

{- why not make eval use step?

because we want to keep an evaluation counter to prevent infinite loops, and
each step should decrement the counter; but what should step do if it encounters
a nested non-terminal sequence?

 - it could do nothing, in which case eval now needs to reïmplement some of
   the step logic in order to determine which subseq to select

 - it could ask eval to terminate the subseq appropriately, but as step
   doesn't care about the evaluation counter we are forced to allow
   unbounded execution (even if each subseq has finite steps, they could
   themselves instantiate subseqs recursively ad infinitum)

thus, rather than code duplicate, we might as well have evaluate implement the
stepping, and its subseq choices, whilst minding the evaluation counter and then
we can implement `step` simply as evaluation with max 1 step

what about nesting? should impart a penalty?? could we construct an infinite
nest of noops?

  fubar = As f fubar

would be problematic, so perhaps each descent should incur a penalty of 1 step
to mitigate this? this would also recover the behaviour I originally envisaged
in which you would have to select the individual subexpressions to step during
manual evaluation mode;

perhaps we could allow prefixing an evaluation instruction with a number to
overcome this limitation?

-}


{-data Expr l a = Stop
              | Perm [Expr l a] [Expr l a]
              | Seq [Expr l a]
              | Label l
              | As l (Expr l a)
              | Ref a deriving (Eq, Ord)-}

-- short-circuiting
eval' :: x ~ (Maybe Int, Expression) => Ctx -> Direction -> x -> x
eval' c d x@(n, e)
  | maybe False (<= 0) n = x
  | terminated c d e = x
  | otherwise = eval'' c d x

eval'' :: x ~ (Maybe Int, Expression) => Ctx -> Direction -> x -> x
eval'' c d (n, As l e) = As l <$> eval' c d (pred <$> n, e)
eval'' c d x@(n, Ref r) = maybe x go $ fetch c r
  where
    go = (As ("`" ++ fst r ++ suffix) <$>) . eval' c d . (pred <$> n,)
    suffix = if disambiguate c (fst r) == Just r then "" else '_' : show (snd r)
eval'' c d (n, Seq s@(_:_)) = error "unimplemented"
eval'' _ _ (n, e) = (Just (-1), e)
  -- shouldn't get here, as these expressions should be `terminated`

eval :: Maybe Int -> Ctx -> Direction -> Expression -> Expression
eval n c d = snd . eval' c d . (n,)

step :: Ctx -> Direction -> Expression -> Expression
step c = eval (Just 1) c
-- step c d (As l e) = As l . step c d $ e
-- step c d (Ref r) = fromMaybe (Ref r) . As ('`' : fst r) . step c d $ fetch c r
-- step c d 
-- step _ _ e = e

-- unify :: Ctx -> Expression -> Expression -> Bool

-- unify = undefined

equivalent :: Ctx -> Expression -> Expression -> Bool
--equivalent c 
equivalent _ _ _ = False

unify :: m ~ Map ID Expression => Ctx -> m -> Expression -> Expression -> Maybe m
unify c m e (As _ f) = unify c m e f
unify c m e (Ref r) = fetch c r >>= unify c m e

unify _ m Stop Stop = Just m

unify _ m (Perm p p') (Perm q q') = error "unimplemented"

unify c m (Seq (x:xs)) (Seq (y:ys)) = do m' <- unify c m x y
                                         unify c m' (Seq xs) (Seq ys)
unify c m (Seq []) (Seq []) = Just m

--unify c m (Label l) e = 

-- unify c m (Seq s) (Seq t)
--   | length s == length t = foldM (\m -> uncurry $ unify c m) m $ zip s t
--   | otherwise = Nothing

unify _ _ _ _ = Nothing

instructure :: Ctx -> Map ID Expression -> Expression -> Expression
instructure = undefined












