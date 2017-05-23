{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Model
( module Model
) where

import Sigma
import Common
import Output (printx, ShowX)
import qualified Data.Map.Lazy as M
import Control.Monad.Except
import Control.Monad.State

--- sigma zipper

data ZigmaSeq = ZigmaSeq [Sigma] [Sigma] deriving (Show)

data Zigma = Zigma [ZigmaSeq] Sigma deriving (Show)

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

type EvalState a = ExceptT EvalError (State EvalCtx) a

--

remguard :: EvalState ()
remguard = do c <- remaining <$> get
              unless (c>0) $ throwError IncompleteComputation

deplete :: EvalState ()
deplete = remguard >> modify (\c -> c {remaining = pred $ remaining c})

replete :: ExtendedNat -> EvalState ()
replete n = modify $ \c -> c {remaining = n}

--

getit :: EvalState Sigma
getit = zget . it <$> get

putit :: Sigma -> EvalState ()
putit s = modify $ \c -> c {it = zput s $ it c}

geteq :: Int -> EvalState Int
geteq n = (M.! n) . eqcls . context <$> get

--

modas :: (M.Map ID Sigma -> M.Map ID Sigma) -> EvalState ()
modas f = modify $ \c -> c {assignments = f $ assignments c}

putas :: M.Map ID Sigma -> EvalState ()
putas a = modify $ \c -> c {assignments = a}

locas :: EvalState a -> EvalState a
locas f = do { c <- get ; putas M.empty ; x <- f
             ; putas $ assignments c ; return x }

--

withMove :: Breadcrumb -> EvalState a -> EvalState a
withMove b a = move b >> a >>= \x -> move (back b) >> return x

move' :: Breadcrumb -> EvalState Bool
move' b = do (r,z) <- go' b . it <$> get
             modify $ \c -> c {it = z}
             return r

move :: Breadcrumb -> EvalState ()
move b = move' b >>= flip unless (throwError $ MoveError b)

--

showex :: ShowX a Context => a -> EvalState String
showex s = printx s . context <$> get

--

deref' :: Int -> EvalState Sigma
deref' n = (M.! n) . tokens . context <$> get

deref :: EvalState ()
deref = getit >>= \case
          SigmaTok _ n -> deref' n >>= putit
          _ -> return ()

getperm :: Sigma -> EvalState (Maybe Perm)
getperm (SigmaTok _ n) = getperm . (M.! n) . tokens . context =<< get
getperm (SigmaPerm _ n) = M.lookup n . perms . context <$> get
getperm _ = return Nothing

--- evaluation

eval :: Direction -> EvalState ()
eval d = getit >>= \case
           SigmaSeq xs -> evals d xs
           SigmaTok _ _ -> deref >> eval d
           SigmaPerm _ _ -> return ()

evals :: Direction -> [Sigma] -> EvalState ()
evals _ [] = return ()
evals Down (p:_) = evals' p $ evalp Down
evals Up (last -> p) = evals' p . flip $ evalp Up

evals' :: Sigma -> ([Permite] -> [Permite] -> EvalState ()) -> EvalState ()
evals' p f = getperm p >>= maybe (return ()) (\(Perm l r) -> f l r)

evalp :: Direction -> [Permite] -> [Permite] -> EvalState ()
evalp d ls rs = remguard >> locas (unifies ls >> mapM substitute rs
                                              >>= putit . SigmaSeq)
                         >> deplete >> eval d
--

-- evalp d ls rs = locas $ remguard >> get >>= traceM . show
--                                  >> unifies ls
--                                  >> get >>= traceM . show
--                                  >> mapM substitute rs
--                                  >>= putit . SigmaSeq
--                                  >> deplete >> eval d

--- unification

unify :: Permite -> EvalState ()
unify (PermPerm r n) = getit >>= requiv (SigmaPerm r n)
unify (PermLabel l) =
  M.lookup l . assignments <$> get >>= \case
    Nothing -> getit >>= modas . M.insert l
    Just y -> getit >>= requiv y
unify (PermSeq xs)
  | termp Down xs = eval Down >> unifies xs
  | termp Up xs   = eval Up >> unifies xs
  | otherwise     = showex (PermSeq xs) >>= throwError . UnificationError .
                          ("can't unify non-terminal sequence " ++)

unifies :: [Permite] -> EvalState ()
unifies [] = getit >>= \case
               SigmaSeq [] -> return ()
               SigmaSeq _ ->
                 getit >>= showex >>= throwError . UnificationError .
                         ("expecting stop, found " ++)
               SigmaTok _ _ -> deref >> unifies []
               SigmaPerm _ _ -> throwError $
                 UnificationError "expecting stop, found perm"
unifies ps = withMove South $ unifies' ps

-- unify over a list can't be done with a simple fold because
-- we must also move about a zipper, and keep track of whether
-- there are exactly the right number of arguments
unifies' :: [Permite] -> EvalState ()
unifies' (p:ps@(_:_)) =
  do unify p
     b <- move' East
     unless b . throwError $ UnificationError "too few arguments"
     unifies' ps
unifies' [p] =
  do unify p
     b <- move' East
     when b . throwError $ UnificationError "too many arguments"

substitute :: Permite -> EvalState Sigma
substitute (PermPerm r n) = return $ SigmaPerm r n
substitute (PermSeq xs) = SigmaSeq <$> mapM substitute xs
substitute (PermLabel l) =
  M.lookup l . assignments <$> get >>= \case
    Nothing -> throwError . UnificationError $ "can't substitute " ++ l
    Just s -> return s

--- predicates

termp :: Direction -> [Permite] -> Bool
termp _ [] = True
termp Down (PermSeq _ : _) = True
termp Up (last -> PermSeq _) = True
termp _ _ = False

requiv :: Sigma -> Sigma -> EvalState ()
requiv x y = do sx <- showex x
                sy <- showex y
                b <- equivp x y
                unless b . throwError . UnificationError $
                  "Can't unify " ++ sx ++ " with " ++ sy

equivp :: Sigma -> Sigma -> EvalState Bool
equivp (SigmaTok _ n) (SigmaTok _ m)
  | m == n    = return True
  | otherwise = do m' <- deref' m
                   n' <- deref' n
                   equivp m' n'
equivp (SigmaTok _ n) m = deref' n >>= equivp m
equivp n (SigmaTok _ m) = deref' m >>= equivp n

equivp (SigmaSeq []) (SigmaSeq []) = return True
equivp (SigmaSeq (x:xs)) (SigmaSeq (y:ys)) =
  do b <- equivp x y
     b' <- equivp (SigmaSeq xs) (SigmaSeq ys)
     return $ b && b'

equivp (SigmaPerm _ n) (SigmaPerm _ m) =
  do { n' <- geteq n ; m' <- geteq m ; return $ m' == n' }

equivp _ _ = return False
