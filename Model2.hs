{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Model2
( module Model2
) where

import Data.Function (on)
import qualified Data.Map.Lazy as M
import qualified Scope as S
import Common (shows', initlast)
import GHC.Generics (Generic)
import Data.Hashable

type ID = String
type Hash = Int
data Named = ByLabel ID
           | ByRef ID
           | Anon

data Expr = ESeq Named [Expr]
          | EPerm PRef

data PID = PID { signature :: Hash
               , identifier :: Int }

data PRef = Local Perm
          | Remote (S.Context ID Perm)

data Perm = Perm PID [PExpr] [PExpr]

data PExpr = PSeq Named [PExpr]
           | PLabel ID
           | PPerm PRef

--- output

instance Show Expr where
  show (ESeq (ByRef r) _) = "`" ++ r
  show (ESeq (ByLabel l) e) = l ++ "@" ++ show (ESeq Anon e)
  show (ESeq _ e) = "(" ++ shows' e ++ ")"
  show (EPerm p) = show p

instance Show PRef where
  show (Local p) = show p
  show (Remote c) = "`" ++ S.ref c

instance Show Perm where
  show (Perm _ l r) = "<_ " ++ shows' l ++ " : " ++ shows' r ++ " _>"

instance Show PExpr where
  show (PSeq (ByRef r) _) = "`" ++ r
  show (PSeq (ByLabel l) e) = l ++ "@" ++ show (PSeq Anon e)
  show (PSeq _ []) = "#"
  show (PSeq _ (getdat -> Just d)) = showdat d
  show (PSeq _ s) = "(" ++ shows' s ++ ")"
  show (PLabel l) = l
  show (PPerm p) = show p

getdat :: [PExpr] -> Maybe [PExpr]
getdat (initlast -> Just (PSeq _ [] : d, PSeq _ [])) = Just d
getdat _ = Nothing

showdat :: [PExpr] -> String
showdat d = "{" ++ shows' d ++ "}"

--- perm representations

deref :: PRef -> Perm
deref (Local p) = p
deref (Remote c) = S.def c

identify :: Perm -> Int
identify (Perm i _ _) = identifier i

bruijns :: [PExpr] -> M.Map ID Int
bruijns = snd . foldl go (1, M.empty)
  where
    go nm (PSeq _ s) = foldl go nm s
    go (n,m) (PLabel l) =
      case M.lookup l m of
        Nothing -> (n+1, M.insert l n m)
        Just _ -> (n,m)
    go nm _ = nm

--- signature representations

data Sig = Sig [SExpr] [SExpr]
         deriving (Eq, Ord, Generic)

data SExpr = SSeq [SExpr]
           | SLabel Int
           | SPerm
           deriving (Eq, Ord, Generic)

instance Hashable SExpr
instance Hashable Sig

sig :: Perm -> Sig
sig (Perm _ l r) = (Sig `on` map go) l r
  where
    m = bruijns $ l ++ r
    go (PSeq _ s) = SSeq $ map go s
    go (PLabel l') = SLabel $ m M.! l'
    go (PPerm _) = SPerm

slots :: Perm -> [Int]
slots (Perm _ l r) = concatMap go $ l ++ r
  where
    go (PSeq _ s) = concatMap go s
    go (PLabel _) = []
    go (PPerm (deref -> p)) = [identify p]
