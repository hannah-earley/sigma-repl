module Sigma
( module Sigma
) where

import qualified Map as M

--- sigma data types

data Ref = Anonymous | ByName String

data Sigma = SigmaSeq [Sigma]
           | SigmaTok String Int
           | SigmaPerm Ref Int

data Permite = PermSeq [Permite]
             | PermLabel String
             | PermPerm Ref Int

data Perm = Perm [Permite] [Permite]

data Context = Context { it :: Sigma
                       , tokens :: M.Map Int Sigma
                       , perms :: M.Map Int Perm
                       , eqcls :: M.Map Int Int
                       , overture :: M.Map String Int }

--- uniquification

data Sig = Sig [Sigite] [Sigite]

data Sigite = SigSeq [Sigite]
            | SigLab Int
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
    go (PermLabel l') = SigLabel $ m M.! l'
    go (PermPerm _) = SigPerm

slots :: Perm -> [Int]
slots (Perm l r) = concatMap go $ l ++ r
  where
    go (PermSeq s) = concatMap go s
    go (PermLabel _) = []
    go (PermPerm _ n) = [n]
