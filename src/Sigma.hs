{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Sigma
( module Sigma
) where

import Common (ID, ReadError(..))
import qualified Parser as P
import qualified Graph as G
import qualified Data.Map as M

import Control.Exception (throwIO)
import Control.Monad.State

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

data Context = Context { tokens :: M.Map Int Sigma
                       , perms :: M.Map Int Perm
                       , eqcls :: M.Map Int Int
                       , overture :: M.Map ID Int }

empty = Context M.empty M.empty M.empty M.empty

---

type Xified a = StateT (G.Graph, [Int], M.Map Int Perm) IO a

withOverture :: Xified a -> Xified a
withOverture f =
  do { (g, ns, m) <- get ; put (G.overroot g, ns, m) ; x <- f
     ; (_, ns', m') <- get ; put (g, ns', m') ; return x }

insertAnon :: Perm -> Xified Int
insertAnon p = do (g, n:ns, m) <- get
                  put (g, ns, M.insertWith err n p m)
                  return n
  where err = error "anonymous perm redeclaration"

permitify :: P.SigmaToken -> Xified Permite
permitify (P.SigmaSeq xs) = PermSeq <$> mapM permitify xs
permitify (P.SigmaLabel l) = return $ PermLabel l
permitify (P.SigmaRef' r) = withOverture . permitify $ P.SigmaRef r
permitify (P.SigmaRef r) =
  do (g, _, _) <- get
     case G.search g r of
       [] -> liftIO . throwIO $ ReferenceError r
       (n, P.SigmaPerm _ _) : _ -> return $ PermPerm (ByName r) n
       _ -> liftIO . throwIO $
              OtherError "Perms may only reference other perms"
permitify (P.SigmaPerm ls rs) =
  PermPerm Anonymous <$> (permify (ls,rs) >>= insertAnon)

permify :: ([P.SigmaToken], [P.SigmaToken]) -> Xified Perm
permify (ls,rs) =
  do ls' <- mapM permitify ls
     rs' <- mapM permitify rs
     return $ Perm ls' rs'

sigmify :: P.SigmaToken -> Xified Sigma
sigmify (P.SigmaSeq xs) = SigmaSeq <$> mapM sigmify xs
sigmify (P.SigmaLabel l) = sigmify (P.SigmaRef l)
sigmify (P.SigmaRef' r) = withOverture . sigmify $ P.SigmaRef r
sigmify (P.SigmaRef r) =
  do (g, _, _) <- get
     case G.search g r of
       [] -> liftIO . throwIO $ ReferenceError r
       (n, P.SigmaPerm _ _) : _ -> return $ SigmaPerm (ByName r) n
       (n, _) : _ -> return $ SigmaTok r n
sigmify (P.SigmaPerm ls rs) =
  SigmaPerm Anonymous <$> (permify (ls,rs) >>= insertAnon)

-- permitify :: G.Graph -> [Int] -> P.SigmaToken -> (Permite, M.Map Int Perm, [Int])
-- permitify = undefined
--
-- permify :: t ~ [P.SigmaToken] => G.Graph -> [Int] -> (t,t) -> (Perm, M.Map Int Perm, [Int])
-- permify g ns (l,r) =
--   case permitify g ns (P.SigmaSeq l) of
--     (PermSeq l', m, ns') ->
--       case permitify g ns' (P.SigmaSeq r) of
--         (PermSeq r', m', ns'') ->
--           (Perm l' r', M.unionWith anonerr m m', ns'')
--
-- sigmify :: G.Graph -> [Int] -> P.SigmaToken -> (Sigma, M.Map Int Perm, [Int])
-- sigmify g ns (P.SigmaSeq xs) = wrap $ foldr f ([], M.empty, ns) xs
--   where
--     f x (ys, m, ns') =
--       case sigmify g ns' x of
--         (y, m', ns'') -> (y:ys, M.unionWith anonerr m m', ns'')
--     wrap (ys, m, ns') = (SigmaSeq ys, m, ns')
-- sigmify g ns (P.SigmaLabel l) = sigmify g ns (P.SigmaRef l)
-- sigmify g ns (P.SigmaRef r) =
--   (, M.empty, ns) $
--   case G.search g r of
--     [] -> error ""
--     (n, P.SigmaPerm _ _) : _ -> SigmaPerm (ByName r) n
--     (n, _) : _ -> SigmaTok r n
-- sigmify g ns (P.SigmaRef' r) = sigmify (G.overroot g) ns (P.SigmaRef r)
-- sigmify g ns (P.SigmaPerm l r) =
--   case permify g ns (l,r) of
--     (p, m, n:ns') -> (SigmaPerm Anonymous n, M.insertWith anonerr n p m, ns')

--- contextualise

-- permify :: t ~ P.SigmaToken -> (t,t) -> [Int] -> (Perm, M.Map Int Perm)
-- permify (ls,rs) ns = undefined
--   where
--     permify' (P.SigmaSeq ts) ns = undefined
--     permify' (P.SigmaLabel l) ns = (PermLabel l, M.empty, ns)
--     permify' (P.SigmaRef )
--
--
-- sigmify :: P.SigmaToken -> [Int] -> (Sigma, M.Map Int Perm)
-- sigmify
--
-- insertAnon :: Context -> P.SigmaToken -> (Context, Int)
-- insertAnon = undefined

-- sigmify :: G.Graph -> Context -> P.SigmaToken ->
--
-- insert :: G.Graph -> Context -> Int -> P.SigmaToken -> Context
-- insert g c n (P.SigmaSeq xs) =
--   where
--     m = M.keys $ perms c
--
-- contextualise :: G.Graph -> Context
-- contextualise g =


-- contextualise :: G.Graph -> P.SigmaToken -> Context
-- contextualise = undefined

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
