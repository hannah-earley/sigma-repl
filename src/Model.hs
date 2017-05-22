{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Model
( module Model
) where

import Sigma
import Common (ID, ExtendedNat, initlast)
import Control.Monad.State
import qualified Data.Map.Lazy as M

--- Sigma Zipper

data ZigmaSeq = ZigmaSeq [Sigma] [Sigma]
data Zigma = Zigma [ZigmaSeq] Sigma
data Breadcrumb = North | East | South | West

back :: Breadcrumb -> Breadcrumb
back North = South
back East = West
back South = North
back West = East

tozip :: Sigma -> Zigma
tozip = Zigma []

fromzip :: Zigma -> Sigma
fromzip = zget . goend North

go :: Breadcrumb -> Zigma -> Zigma
go b = snd . go' b

goend :: Breadcrumb -> Zigma -> Zigma
goend b = snd . until (not . fst) (go' b . snd) . (True,)

go' :: Breadcrumb -> Zigma -> (Bool, Zigma)
go' North (Zigma (ZigmaSeq ls rs : zs) r) =
  (True, Zigma zs . SigmaSeq $ foldl (flip (:)) (r:rs) ls)
go' East (Zigma (ZigmaSeq ls (r:rs) : zs) l) =
  (True, Zigma (ZigmaSeq (l:ls) rs : zs) r)
go' West (Zigma (ZigmaSeq (l:ls) rs : zs) r) =
  (True, Zigma (ZigmaSeq ls (r:rs) : zs) l)
go' South (Zigma zs (SigmaSeq (x:xs))) =
  (True, Zigma (ZigmaSeq [] xs : zs) x)
go' _ z = (False, z)

zget :: Zigma -> Sigma
zget (Zigma _ s) = s

zput :: Sigma -> Zigma -> Zigma
zput s (Zigma z _) = Zigma z s

--- evaluation context & manipulation

data EvalCtx = EvalCtx { remaining :: ExtendedNat
                       , context :: Context
                       , assignments :: M.Map ID Sigma
                       , it :: Zigma }

data EvalError = IncompleteComputation
               | UnificationError String
               | MoveError Breadcrumb

-- type EvalState a = EitherT EvalError (State EvalCtx) a
type EvalState a = State EvalCtx a

deplete :: EvalState ()
deplete = modify $ \c -> c {remaining = pred $ remaining c}

getit :: EvalState Sigma
getit = zget . it <$> get

putit :: Sigma -> EvalState ()
putit s = modify $ \c -> c {it = zput s $ it c}

-- modit :: (Zigma -> Zigma) -> EvalState ()
-- modit f = modify $ \c -> c {it = f $ it c}

assign :: M.Map ID Sigma -> EvalState ()
assign a = modify $ \c -> c {assignments = a}

assignLocal :: EvalState a -> EvalState a
assignLocal f = do { c <- get ; assign M.empty ; x <- f
                   ; assign $ assignments c ; return x }

withMove :: Breadcrumb -> EvalState a -> EvalState a
withMove b a = move b >> a >>= \x -> move (back b) >> return x

move :: Breadcrumb -> EvalState ()
move b = do (r,z) <- go' b . it <$> get
            unless r $ fail "move error"
            modify $ \c -> c {it = z}

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
eval'' ls rs = do unifies ls
                  rs' <- mapM substitute rs
                  putit $ SigmaSeq rs'
                  deplete

unifies :: [Permite] -> EvalState ()
unifies [] = getit >>= \case
               SigmaSeq [] -> return ()
               SigmaTok _ _ -> deref >> unifies []
               SigmaPerm _ _ -> fail "unification error"
unifies ps = withMove South $ unifies' ps

unifies' :: [Permite] -> EvalState ()
unifies' (p:ps@(_:_)) = unify p >> move East >> unifies' ps
unifies' [p] = do unify p
                  (b,_) <- go' East . it <$> get
                  when b $ fail "unification error"

unify :: Permite -> EvalState ()
unify = undefined

substitute :: Permite -> EvalState Sigma
substitute = undefined
