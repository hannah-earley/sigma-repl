{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Output
( printx
) where

import Sigma
import Common (ID, initlast)
import Graph (search)

import Control.Monad.State
import qualified Data.Map.Lazy as M

--- stateful printing

printx :: ShowX a Context => a -> Context -> String
printx = evalState . showx

class ShowX a x where
  showx :: a -> State x String

showsx :: ShowX a x => [a] -> State x String
showsx = fmap unwords . mapM showx

--- core rendering

instance ShowX Perm Context where
  showx (Perm ls rs) =
    do ls' <- showsx ls
       rs' <- showsx rs
       return $ "<" ++ ls' ++ " : " ++ rs' ++ ">"

instance ShowX Permite Context where
  showx (PermSeq []) = return "#"
  showx (PermSeq s) =
    getdat s >>= \case
      Left p -> ("(" ++) . (++ ")") <$> showsx p
      Right d -> showdat d
  showx (PermLabel l) = return l
  showx (PermPerm (ByName r) _) = return r
  showx (PermPerm Anonymous n) =
    showx =<< (M.! n) . perms <$> get

instance ShowX Sigma Context where
  showx = showx . s2p

s2p :: Sigma -> Permite
s2p (SigmaSeq xs) = PermSeq $ map s2p xs
s2p (SigmaTok l _) = PermLabel l
s2p (SigmaPerm r n) = PermPerm r n

--- syntactical resugaring

data Dat = DatCons Permite Permite | DatNil
         | DatSucc Permite | DatZero
         | DatDat [Permite]

ocls :: ID -> State Context Int
ocls r = do (o,c) <- liftM2 (,) overture eqcls <$> get
            return $ c M.! (fst . head $ search o r)

getdat :: [Permite] -> State Context (Either [Permite] Dat)
getdat [PermSeq [], x'@(PermPerm _ x), y, z, PermSeq []] =
  do c <- eqcls <$> get
     [cc,nc] <- mapM ocls ["cons", "nil"]
     return . Right $ getdatl x' (c M.! x) cc nc y z
getdat [PermSeq [], x'@(PermPerm _ x), y, PermSeq []] =
  do c <- eqcls <$> get
     [sc,zc] <- mapM ocls ["succ", "zero"]
     return . Right $ getdatn x' (c M.! x) sc zc y
getdat (initlast -> Just (PermSeq [] : d, PermSeq [])) =
  return . Right $ DatDat d
getdat s = return $ Left s

getdatl :: Permite -> Int -> Int -> Int -> Permite -> Permite -> Dat
getdatl _ x cc _ y z | x == cc = DatCons y z
getdatl _ x _ nc (PermSeq[]) (PermSeq[]) | x == nc = DatNil
getdatl x _ _ _ y z = DatDat [x,y,z]

getdatn :: Permite -> Int -> Int -> Int -> Permite -> Dat
getdatn _ x sc _ m | x == sc = DatSucc m
getdatn _ x _ zc (PermSeq[]) | x == zc = DatZero
getdatn x _ _ _ y = DatDat [x,y]

showdat :: Dat -> State Context String
showdat (DatCons x y) =
  do xs <- showx x
     ys <- showdatl y
     return $ "[" ++ unwords (xs : ys) ++ "]"
showdat DatNil = return "[]"
showdat (DatSucc n) = showdatn 1 n
showdat DatZero = return "0"
showdat (DatDat d) = ("{" ++) . (++ "}") <$> showsx d

showdatl :: Permite -> State Context [String]
showdatl x@(PermSeq x') =
  getdat x' >>= \case
    Right (DatCons y z) ->
     do ys <- showx y
        zs <- showdatl z
        return $ ys : zs
    Right DatNil -> return []
    _ -> (:[]) . (". " ++) <$> showx x
showdatl x = (:[]) . (". " ++) <$> showx x

showdatn :: Int -> Permite -> State Context String
showdatn n x@(PermSeq x') =
  getdat x' >>= \case
    Right (DatSucc m) -> showdatn (n+1) m
    Right (DatZero) -> return $ show n
    _ -> ((++) $ "{#" ++ show n ++ " . ") . (++ "}") <$> showx x
showdatn n x = ((++) $ "{#" ++ show n ++ " . ") . (++ "}") <$> showx x
