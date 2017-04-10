{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Moldel
( module Moldel
) where

import Common (acyclicp, shows', initlast)
import qualified Scope as S
import qualified Data.Map.Lazy as M
import Data.List (nub)
import Control.Monad (foldM, join, guard)
import Data.Function (on)

--- nice names

type ID = String
type Hash = Int
type Expr = ETerm

--- references

data Ref = Ref (Maybe Hash) Def

instance Show Ref where
  show (Ref _ d) = show d

data Def = Anon Expr
         | Ext (S.Context ID Expr)

instance Show Def where
  show (Anon e) = show e
  show (Ext c) = "`" ++ S.ref c

--- sequence expression

data ETerm = ESeq [ETerm]
           | EPerm Perm
           | ERef Ref

instance Show ETerm where
 show = show . lift

lift :: ETerm -> ATerm
lift (ESeq ts) = ASeq $ map lift ts
lift (EPerm p) = APerm' p
lift (ERef r) = ARef r

--- unified permutation

data Perm = Perm APerm FPerm

instance Show Perm where
  show (Perm p _) = show p

--- permutation with as-expressions (for display)

data APerm = APerm [ATerm] [ATerm]

instance Show APerm where
  show (APerm l r) = "<_ " ++ shows' l ++ " : " ++ shows' r ++ " _>"

data ATerm = ASeq [ATerm]
           | ALabel ID (Maybe ATerm)
           | APerm' Perm
           | ARef Ref

pattern AStop :: ATerm
pattern AStop = ASeq []

instance Show ATerm where
  show AStop = "#"
  show (ASeq (initlast -> Just (AStop : d, AStop))) = showdat d
  show (ASeq s) = "(" ++ unwords (map show s) ++ ")"
  show (ALabel l Nothing) = l
  show (ALabel l (Just e)) = l ++ "@" ++ show e
  show (APerm' p) = show p
  show (ARef r) = show r

showdat :: [ATerm] -> String
showdat d = "{" ++ shows' d ++ "}"

--- compile as-perm to flat-perm

pcompile :: APerm -> Maybe Perm
pcompile p = Perm p <$> flatten p

build :: M.Map ID ATerm -> ATerm -> Maybe (M.Map ID ATerm)
build m (ASeq s) = foldM build m s
build m (ALabel l (Just e)) =
  case M.lookup l m of
    Just _ -> Nothing
    Nothing -> build (M.insert l e m) e
build m _ = Just m

depgraph :: M.Map ID ATerm -> [(ID, ID, [ID])]
depgraph = map (uncurry $ join (,,)) . M.toList . M.map (nub . go)
  where
    go (ASeq s) = concatMap go s
    go (ALabel l _) = [l]
    go _ = []

flatten :: APerm -> Maybe FPerm
flatten (APerm l r) = do m <- foldM build M.empty $ l ++ r
                         guard . acyclicp $ depgraph m
                         return $ flatten' m l r

flatten' :: M.Map ID ATerm -> [ATerm] -> [ATerm] -> FPerm
flatten' m = FPerm `on` map go
  where
    go (ASeq s) = FSeq $ map go s
    go (ALabel l' _) = maybe (FLabel l') go $ M.lookup l' m
    go (APerm' p) = FPerm' p
    go (ARef r) = FRef r

--- 'flat' permutation without as-expressions

data FPerm = FPerm [FTerm] [FTerm]

data FTerm = FSeq [FTerm]
           | FLabel ID
           | FPerm' Perm
           | FRef Ref

bruijns :: [FTerm] -> M.Map ID Int
bruijns = snd . foldl go (1,M.empty)
  where
    go nm (FSeq s) = foldl go nm s
    go (n,m) (FLabel l') =
      case M.lookup l' m of
        Nothing -> (n+1, M.insert l' n m)
        Just _ -> (n,m)
    go nm _ = nm

--- signature sequence

data STerm = SSeq [STerm]
           | SLabel Int
           | SPerm [STerm] [STerm]
           | SRef Ref

instance Eq STerm where
  SSeq xs == SSeq ys = xs == ys
  SLabel m == SLabel n = m == n
  SPerm a b == SPerm c d = a == b && c == d
  SRef _ == SRef _ = True

sige' :: ETerm -> STerm
sige' = sigp' . lift

sigp' :: ATerm -> STerm
sigp' (ASeq s) = SSeq $ map sigp' s
sigp' (APerm' (Perm _ f)) = sigf' f
sigp' (ARef r) = SRef r

sigf' :: FPerm -> STerm
sigf' (FPerm l r) = let m = bruijns $ l ++ r
                    in on SPerm (map $ go m) l r
  where
    go m (FSeq s) = SSeq $ map (go m) s
    go m (FLabel (flip M.lookup m -> Just n)) = SLabel n
    go _ (FPerm' (Perm _ f)) = sigf' f
    go _ (FRef r') = SRef r'
