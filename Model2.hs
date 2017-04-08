{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Model2
( module Model2
) where

import qualified Scope as S
import Common (shows', initlast)

type ID = String
type Hash = Int
type IDLab = Either ID ID

data Expr = ESeq (Maybe ID) [Expr]
          | EPerm Perm

data Perm = Perm Hash Def

data Def = Local [PExpr] [PExpr]
         | Remote (S.Context ID Perm)

data PExpr = PSeq (Maybe IDLab) [PExpr]
           | PLabel ID
           | PPerm Perm

--- output

instance Show Expr where
  show (ESeq (Just r) _) = "`" ++ r
  show (ESeq _ e) = "(" ++ shows' e ++ ")"
  show (EPerm p) = show p

instance Show Perm where
  show (Perm _ p) = show p

instance Show Def where
  show (Local l r) = "<_ " ++ shows' l ++ " : " ++ shows' r ++ " _>"
  show (Remote c) = "`" ++ S.ref c

instance Show PExpr where
  show (PSeq (Just (Left i)) _) = "`" ++ i
  show (PSeq (Just (Right l)) e) = l ++ "@" ++ show (PSeq Nothing e)
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
