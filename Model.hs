{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Model
( ID
, Expr(..)
, Expression
, Direction(..)
, bruijns
, eval
, exec
, step
) where

import Module
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Hashable (Hashable, hash)
import Control.Applicative
import Control.Monad

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

stringify :: (Show l, Show r) => Expr l r -> Expr String String
stringify Stop = Stop
stringify p@(Perm _ _) = pmap stringify p
stringify (Seq s) = Seq $ map stringify s
stringify (Label l) = Label $ show l
stringify (As l e) = As (show l) $ stringify e
stringify (Ref r) = Ref $ show r

--- output

instance {-# OVERLAPPING #-} Show Expression where
  show = show . reslot fst

instance {-# OVERLAPPING #-} Show (Expr String String) where
  show Stop = "#"
  show (Perm l r) = "<_ " ++ showseq l ++ " : " ++ showseq r ++ " _>"
  show (Seq (initlast -> Just ((Stop : d), Stop))) = showdat d
  show (Seq s) = "(" ++ showseq s ++ ")"
  show (Label l) = l
  show (As l e) = l ++ "@" ++ show e
  show (Ref r) = '`' : r

instance {-# OVERLAPPABLE #-} (Show l, Show r) => Show (Expr l r) where
  show = show . stringify

showseq = unwords . map show

pattern DataSeq :: Expr ID ID -> Expr ID ID -> Expr ID ID
pattern DataSeq a b <- Seq [Stop, a, b, Stop]

showdat [Ref "cons", DataSeq x y] = "[" ++ unwords (show x : showdatl y) ++ "]"
showdat [Ref "nil", Stop] = "[]"
showdat [Ref "succ", n] = showdatn 1 n
showdat [Ref "zero", _] = "0"
showdat d = "{" ++ showseq d ++ "}"

showdatl (DataSeq (Ref "cons") (DataSeq x y)) = show x : showdatl y
showdatl (DataSeq (Ref "nil") Stop) = []
showdatl x = [". " ++ show x]

showdatn :: Int -> Expr ID ID -> String
showdatn n (DataSeq (Ref "succ") m) = showdatn (n+1) m
showdatn n (DataSeq (Ref "zero") Stop) = show n
showdatn n x = "{#" ++ show n ++ " . " ++ show x ++ "}"

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

pmap :: (Expr l a -> Expr m b) -> Expr l a -> Expr m b
pmap f (Perm l r) = Perm (map f l) (map f r)

instance (Hashable l, Ord l) => Aliasable (Expr l) where
    type Sig (Expr l) = Expr [Int] [Int]

    sig = sig' SigExpr M.empty

    slots (Perm l r) = slots' l ++ slots' r
    slots (Seq xs) = slots' xs
    slots (As _ x) = slots x
    slots (Ref r) = [r]
    slots _ = []

    reslot f p@(Perm _ _) = flip pmap p $ reslot f
    reslot f (Seq xs) = Seq $ map (reslot f) xs
    reslot f (As l x) = As l $ reslot f x
    reslot f (Ref r) = Ref $ f r
    reslot _ (Label l) = Label l
    reslot _ Stop = Stop

    slotp (Ref r) = Just r
    slotp _ = Nothing

slots' :: (Hashable l, Ord l) => [Expr l a] -> [a]
slots' = concatMap slots

sig' :: (Hashable l, Ord l) => SigCtx -> Map l [Int] -> Expr l a -> Expr [Int] [Int]
sig' _ _ Stop = Stop
sig' _ _ (Ref _) = Ref []
sig' c m (Seq x) = Seq $ map (sig' c m) x

sig' _ _ p@(Perm l r) = flip pmap p . sig' SigPerm
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

eval :: Int -> Ctx -> Direction -> Expression -> Expression
eval n c d = snd . eval' c d . (Just n,)

exec :: Ctx -> Direction -> Expression -> Expression
exec c d = snd . eval' c d . (Nothing,)

step :: Ctx -> Direction -> Expression -> Expression
step c = eval 1 c

-- short-circuiting
eval' :: x ~ (Maybe Int, Expression) => Ctx -> Direction -> x -> x
eval' c d x@(n, e)
  | maybe False (<= 0) n = x
  | terminated c d e     = x
  | otherwise            = eval'' c d x

eval'' :: x ~ (Maybe Int, Expression) => Ctx -> Direction -> x -> x
eval'' c d (pn -> n, As l e) = As l <$> eval' c d (n, e)
eval'' _ _ (n, Ref r) = (n, Ref r)
eval'' c d (pn -> n, s@(ps c d n -> Just ((n',a,b), (p,xs)))) =
  let finish = (permjoin d p . flip map b . substitute <$>)
  in eval' c d . maybe (Just 0, s) finish $ unifold c a xs (n', M.empty)
eval'' _ _ (_, e) = (Just (-1), e)
  -- shouldn't get here, as these expressions should be `terminated`

fetch :: Ord r => Context (Expr l) r -> (r, Int) -> Maybe (Expr l (r, Int))
fetch = flip M.lookup . symbols

terminus :: Ord r => Context (Expr l) r -> Expr l (r, Int) -> Bool
terminus _ (Perm _ _) = False
terminus c (As _ e)   = terminus c e
terminus c (Ref r)    = maybe True (terminus c) $ fetch c r
terminus _ _          = True

terminated :: Ord r => Context (Expr l) r -> Direction -> Expr l (r, Int) -> Bool
terminated c d (As _ e)         = terminated c d e
terminated c d (Ref r)          = maybe True (terminated c d) $ fetch c r
terminated c Down (Seq (x : _)) = terminus c x
terminated c Up (Seq s@(_ : _)) = terminus c $ last s
terminated _ _ _                = True

--- useful viewpattern functions

initlast :: [a] -> Maybe ([a],a)
initlast = foldr go Nothing
  where
    go x Nothing = Just ([],x)
    go x (Just (is,l)) = Just (x:is,l)

pn = (pred <$>)

seqsplit Down (p : xs) = Just (p, xs)
seqsplit Up (initlast -> Just (xs, p)) = Just (p, xs)
seqsplit _ _ = Nothing

permify n c Down (reduce c . (n,) -> (n', Perm l r)) = Just (n', l, r)
permify n c Up (reduce c . (n,) -> (n', Perm l r)) = Just (n', r, l)
permify _ _ _ _ = Nothing

ps c d n (Seq (seqsplit d -> Just y@(x,_))) = (,y) <$> permify n c d x
ps _ _ _ _ = Nothing

permjoin Down p xs = Seq (xs ++ [p])
permjoin Up p xs = Seq (p : xs)

---

equivify :: m ~ Map ID Expression => m -> Ctx -> Expression -> Expression -> Maybe m
equivify m c e e' = if equivalentp c e e' then Just m else Nothing

reduce :: x ~ (Maybe Int, Expression) => Ctx -> x -> x
reduce _ x@(n, _) | maybe False (<0) n = x
reduce c (pn -> n, As _ e) = reduce c (n, e)
reduce c (n, Ref r) = maybe (n, Ref r) next $ fetch c r
  where
    next x@(As _ _) = reduce c (n, x)
    next x = (n, x)
reduce _ x = x

unify :: m ~ (Maybe Int, Map ID Expression) => Ctx -> Expression -> Expression -> m -> Maybe m
unify c s@(Seq xs@(_:_)) e (n,m) = go Down <|> go Up
  where
    go d = do guard . terminated c d $ s
              (n', Seq ys) <- return . eval' c d . reduce c $ (n, e)
              guard . terminated c d $ Seq ys
              unifold c xs ys (n',m)

unify _ (Label "_") _ nm = Just nm

unify c (Label l) e (n,m) = (n,) <$> maybe (return $ M.insert l e m)
                                           (equivify m c e) (M.lookup l m)

unify c (As l e) e' nm = unify c (Label l) e' nm >>= unify c e e'

unify c e e' (n,m) = (n,) <$> equivify m c e e'

unifold :: m ~ (Maybe Int, Map ID Expression) => Ctx -> [Expression] -> [Expression] -> m -> Maybe m
unifold c (x:xs) (y:ys) nm = unify c x y nm >>= unifold c xs ys
unifold _ [] [] nm = Just nm
unifold _ _ _ _ = Nothing

substitute :: Map ID Expression -> Expression -> Expression
substitute m (Label l) = M.findWithDefault (Label l) l m
substitute m (As l e) = M.findWithDefault (substitute m e) l m
substitute m (Seq s) = Seq $ map (substitute m) s
substitute _ e = e
