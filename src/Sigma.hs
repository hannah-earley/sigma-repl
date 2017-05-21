module Sigma
( module Sigma
) where

import Common (ID)
import qualified Parser as P
import qualified Graph as G
import qualified Data.Map as M

import Data.Function (on)

--- definitions

data Ref = Anonymous | ByName ID

data Sigma = SigmaSeq [Sigma]
           | SigmaTok ID Int
           | SigmaPerm Ref Int

data Permite = PermSeq [Permite]
             | PermLabel ID
             | PermPerm Ref Int

data Perm = Perm [Permite] [Permite]

data Context = Context { it :: Sigma
                       , tokens :: M.Map Int Sigma
                       , perms :: M.Map Int Perm
                       , eqcls :: M.Map Int Int
                       , overture :: M.Map ID Int }

---

--- contextualise

contextualise :: G.Graph -> P.SigmaToken -> Context
contextualise = undefined

--- uniquification

data Sig = Sig [Sigite] [Sigite]

data Sigite = SigSeq [Sigite]
            | SigLabel Int
            | SigPerm

bruijns :: [Permite] -> M.Map String Int
bruijns = snd . foldl go (1, M.empty)
  where
    go nm (PermSeq s) = foldl go nm s
    go (n,m) (PermLabel l) =
      case M.lookup l m of
        Nothing -> (n+1, M.insert l n m)
        Just _ -> (n,m)
    go nm _ = nm

sig :: Perm -> Sig
sig (Perm l r) = (Sig `on` map go) l r
  where
    bs = bruijns $ l ++ r
    go (PermSeq s) = SigSeq $ map go s
    go (PermLabel l') = SigLabel $ bs M.! l'
    go (PermPerm _ _) = SigPerm

slots :: Perm -> [Int]
slots (Perm l r) = concatMap go $ l ++ r
  where
    go (PermSeq s) = concatMap go s
    go (PermLabel _) = []
    go (PermPerm _ n) = [n]
