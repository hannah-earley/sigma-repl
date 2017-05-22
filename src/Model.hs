{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Model
( module Model
) where

import Sigma
import Common (ID, ExtendedNat, initlast)
import Control.Monad.State
import qualified Data.Map.Lazy as M

--- evaluation context & manipulation

data EvalCtx = EvalCtx { remaining :: ExtendedNat
                       , context :: Context
                       , assignments :: M.Map ID Sigma }

type EvalState a = State EvalCtx a

deplete :: EvalState ()
deplete = modify $ \c -> c {remaining = pred $ remaining c}

getit :: EvalState Sigma
getit = it . context <$> get

putit :: Sigma -> EvalState ()
putit = modify . modcx . setit
setit s c = c {it = s}
modcx f c = c {context = f $ context c}

assign :: M.Map ID Sigma -> EvalState ()
assign a = modify $ \c -> c {assignments = a}

assignLocal :: EvalState a -> EvalState a
assignLocal f = do { c <- get ; assign M.empty ; x <- f
                   ; assign $ assignments c ; return x }

foldSeq :: (a -> EvalState a) -> a -> EvalState a
foldSeq f z = do s <- getit
                 y <- case s of
                   SigmaSeq xs -> foldSeq' xs z
                   _ -> f z
                 putit s
                 return y
  where
    foldSeq' [] z' = return z'
    foldSeq' (x:xs) z' = putit x >> f z' >>= foldSeq' xs

zipSeq :: (a -> EvalState b) -> [a] -> EvalState [b]
zipSeq f as = do s <- getit
                 y <- case s of
                   SigmaSeq xs -> zipSeq' as xs
                   _ -> zipSeq' as [s]
                 putit s
                 return y
  where
    zipSeq' (z:zs) (x:xs) = putit x >> f z >>= (<$> zipSeq' zs xs) . (:)
    zipSeq' _ _ = return []

--- evaluation functions

data Direction = Down | Up

deref :: EvalState ()
deref = getit >>= \case
          SigmaTok _ n -> putit . (M.! n) . tokens . context =<< get
          _ -> return ()

getperm :: Sigma -> EvalState (Maybe Perm)
getperm (SigmaTok _ n) = getperm . (M.! n) . tokens . context =<< get
getperm (SigmaPerm _ n) = M.lookup n . perms . context <$> get
getperm _ = return Nothing

eval :: Direction -> EvalState ()
eval d = getit >>= \case
           SigmaSeq xs -> eval' d xs
           SigmaTok _ _ -> deref >> eval d
           SigmaPerm _ _ -> return ()

eval' :: Direction -> [Sigma] -> EvalState ()
eval' Down (p:_) =
  getperm p >>= \case
    Just (Perm l r) -> eval'' l r
    Nothing -> return ()
eval' Up (initlast -> Just (_,p)) =
  getperm p >>= \case
    Just (Perm l r) -> eval'' r l
    Nothing -> return ()
eval' _ _ = return ()

eval'' :: [Permite] -> [Permite] -> EvalState ()
eval'' l r = do zipSeq unify l
                r' <- mapM substitute r
                putit $ SigmaSeq r'
                deplete

unify :: Permite -> EvalState ()
unify = undefined

substitute :: Permite -> EvalState Sigma
substitute = undefined
