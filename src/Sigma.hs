{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Sigma
( Ref(..)
, Sigma(..)
, Permite(..)
, Perm(..)
, Context(..)
, contextualise
) where

import Common (ID, ReadError(..))
import qualified Parser as P
import qualified Graph as G
import qualified Data.Map as M
import qualified Algorithm as A

import Control.Exception (throwIO)
import Control.Monad.State

import Data.Either (partitionEithers)
import Data.Function (on)

--- definitions

data Ref = Anonymous | ByName ID deriving (Show)

data Sigma = SigmaSeq [Sigma]
           | SigmaTok ID Int
           | SigmaPerm Ref Int
           deriving (Show)

data Permite = PermSeq [Permite]
             | PermLabel ID
             | PermPerm Ref Int
             deriving (Show)

data Perm = Perm [Permite] [Permite] deriving (Show)

data Context = Context { it :: Sigma
                       , tokens :: M.Map Int Sigma
                       , perms :: M.Map Int Perm
                       , eqcls :: M.Map Int Int
                       , overture :: G.Graph } deriving (Show)

--- compilation / contextualisation

type Xified a = StateT (G.Graph, [Int], M.Map Int Perm) IO a

withRoot :: Int -> Xified a -> Xified a
withRoot n f =
  do { og <- getGraph ; modifyGraph $ flip G.reroot n
     ; x <- f ; putGraph og ; return x }

withOverture :: Xified a -> Xified a
withOverture f = G.overture <$> getGraph >>= flip withRoot f

getGraph :: Xified G.Graph
getGraph = do { (g, _, _) <- get ; return g }

putGraph :: G.Graph -> Xified ()
putGraph = modifyGraph . const

modifyGraph :: (G.Graph -> G.Graph) -> Xified ()
modifyGraph f = modify $ \(g, ns, m) -> (f g, ns, m)

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
  do g <- getGraph
     case G.search g r of
       [] -> liftIO . throwIO $ ReferenceError r
       (n, P.SigmaPerm _ _) : _ -> return $ PermPerm (ByName r) n
       _ -> liftIO . throwIO $
              OtherError "Perms may only reference other perms"
permitify (P.SigmaPerm ls rs) =
  PermPerm Anonymous <$> (permify ls rs >>= insertAnon)

permify :: [P.SigmaToken] -> [P.SigmaToken] -> Xified Perm
permify ls rs =
  do ls' <- mapM permitify ls
     rs' <- mapM permitify rs
     return $ Perm ls' rs'

sigmify :: P.SigmaToken -> Xified Sigma
sigmify (P.SigmaSeq xs) = SigmaSeq <$> mapM sigmify xs
sigmify (P.SigmaLabel l) = sigmify (P.SigmaRef l)
sigmify (P.SigmaRef' r) = withOverture . sigmify $ P.SigmaRef r
sigmify (P.SigmaRef r) =
  flip G.search r <$> getGraph >>= \case
    [] -> liftIO . throwIO $ ReferenceError r
    (n, P.SigmaPerm _ _) : _ -> return $ SigmaPerm (ByName r) n
    (n, _) : _ -> return $ SigmaTok r n
sigmify (P.SigmaPerm ls rs) =
  SigmaPerm Anonymous <$> (permify ls rs >>= insertAnon)

detokifies :: Xified (M.Map Int Sigma, M.Map Int Perm)
detokifies =
  do sps <- mapM go . G.defs =<< getGraph
     let (ss, ps) = partitionEithers sps
     return (M.fromList ss, M.fromList ps)
  where
    go (n,t) = liftEither . (n,) <$> withRoot n (detokify t)
    liftEither (n, Left a) = Left (n, a)
    liftEither (n, Right b) = Right (n, b)

detokify :: P.SigmaToken -> Xified (Either Sigma Perm)
detokify (P.SigmaPerm ls rs) = Right <$> permify ls rs
detokify t = Left <$> sigmify t

contextualise' :: P.SigmaToken -> Xified Context
contextualise' p =
  do (tokens', perms') <- detokifies
     it' <- sigmify p
     (g, _, m) <- get
     let perms'' = M.unionWith err perms' m

     return Context { it = it'
                    , tokens = tokens'
                    , perms = perms''
                    , eqcls = uniqify perms''
                    , overture = G.overroot g }
  where err = error "anonymous perm conflict"

contextualise :: G.Graph -> P.SigmaToken -> IO Context
contextualise g p = evalStateT (contextualise' p) (g, [-1,-2..], M.empty)

--- uniqification

data Sig = Sig [Sigite] [Sigite] deriving (Eq, Ord)

data Sigite = SigSeq [Sigite]
            | SigLabel Int
            | SigPerm
            deriving (Eq, Ord)

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

sigslots :: Perm -> (Sig, [Int])
sigslots p = (sig p, slots p)

uniqify :: M.Map Int Perm -> M.Map Int Int
uniqify = A.index . A.dedup . A.duprep . map (fmap sigslots) . M.toList
