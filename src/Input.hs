{-# LANGUAGE TupleSections #-}

module Input
( loadFile
, loadFiles
, loadRaw
) where

import Control.Monad (unless, foldM)
import qualified Control.Exception as E

import Text.Parsec (parse)
import qualified Parser as P
import Graph
import Resource
import Common

import qualified Data.Map as M
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (withCurrentDirectory)

-- could be improved with State...

--- file graphing

doLoad :: Graph -> (Graph -> a -> IO (Graph, Int)) -> a -> IO Graph
doLoad g f x = do now <- getPOSIXTime
                  (g',n') <- f (g {asof = now}) x
                  let (g'',n'') = stack g' n' (root g)
                  return g'' {root = n''}

loadRaw :: Graph -> String -> IO Graph
loadRaw g = doLoad g fetchResource . rawResource

loadFile :: Graph -> FilePath -> IO Graph
loadFile g = doLoad g getInheritance

loadFiles :: Graph -> [FilePath] -> IO Graph
loadFiles = foldM loadFile

--- resource graphing

checkResource :: Graph -> ResourceID -> Bool
checkResource _ (Raw _) = True
checkResource g (File _ _ t) = t <= asof g

fetchResource :: Graph -> Resource -> IO (Graph, Int)
fetchResource g r@(Resource f ri _) =
  do unless (checkResource g ri) . E.throwIO $ InconsistencyError f
     case M.lookup ri (resources g) of
       Nothing -> insertResource g r
       Just (n,_) -> return (g, n)

insertResource :: Graph -> Resource -> IO (Graph, Int)
insertResource g (Resource f r c) =
  let (g',m,n) = addPlaceholder g r f
  in case P.parseResult $ parse P.terms f c of
       P.ParseOK ts -> (,m) <$> applyTerms (g',m,n) ts
       P.ParseError e -> E.throwIO $ ParseError e
       P.ParseIncomplete e ->
         case r of
           Raw _ -> E.throwIO $ IncompleteError e
           _ -> E.throwIO $ ParseError e

addPlaceholder :: Graph -> ResourceID -> FilePath -> (Graph, Int, Int)
addPlaceholder g0 r f =
  let (g1,m) = insertNode g0 Group
      (g2,n) = insertNode g1 Group
      g3 = addEdge g2 n (Edge (Qualified "") Neighbourhood m)
      g4 = g3 {resources = M.insert r (m,f) $ resources g3}
  in (g4,m,n)

-- properly handle changing directory for relative imports, including
-- if we choose to allow for library search paths in the future
getInheritance :: Graph -> FilePath -> IO (Graph, Int)
getInheritance g fp = do (d,r) <- locateResource (base g) fp
                         withCurrentDirectory d $ fetchResource g r

--- [term] graphing

applyTerms :: (Graph,Int,Int) -> [P.Term] -> IO Graph
applyTerms (g,m,n) = foldM (applyTerm . (,m,n)) g

--- term graphing

applyTerm :: (Graph,Int,Int) -> P.Term -> IO Graph

applyTerm (g,_,n) (P.InheritAll fp pre) =
  do (g',l) <- getInheritance g fp
     return $ addEdge g' n $ Edge (Qualified pre) Local l

applyTerm (g,_,n) (P.InheritSome fp xs) =
  do (g',l) <- getInheritance g fp
     let xs' = map (\(x,y) -> Edge (Single x y) Local l) xs
     return $ addEdges g' n xs'

applyTerm (g,m,n) P.BequeathAll =
  return $ addEdge g m (Edge (Qualified "") Local n)

applyTerm (g,m,n) (P.BequeathSome xs) =
  return $ addEdges g m $ map (\(x,y) -> Edge (Single y x) Local n) xs

applyTerm (g,_,n) (P.Group ts) =
  let (g',n') = insertNode g Group
      g'' = addEdge g' n' (Edge (Qualified "") Neighbourhood n)
  in applyTerms (g'',n,n') ts

applyTerm (g,_,n) (P.LocalDef x z) =
  let (g',n') = insertNode g (Def x z)
      g'' = addEdge g' n (Edge (Single x x) Local n')
  in return $ addEdge g'' n' (Edge (Qualified "") Neighbourhood n)

applyTerm (g,m,n) (P.BequeathDef (x,y) z) =
  do g' <- applyTerm (g,m,n) (P.LocalDef x z)
     return $ addEdge g' m (Edge (Single y x) Local n)
