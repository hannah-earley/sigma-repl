{-# LANGUAGE TupleSections #-}

module Input
( module Input
) where

--- imports

import Control.Monad (liftM2, unless, guard)
import qualified Control.Exception as E

import Text.Parsec (parse)
import qualified Parser as P

import Numeric (showHex)
import Data.List (intercalate)
import Data.Typeable (Typeable)
import Data.Hashable (hash)
import qualified Data.Map as M

import Data.Time.Clock.POSIX (POSIXTime)
import qualified System.Posix.Files as F
import System.Posix.Types (DeviceID, FileID)
import System.Posix.IO (handleToFd)
import System.Directory (makeAbsolute, withCurrentDirectory)
import System.FilePath.Posix (makeRelative, splitFileName, (<.>))
import System.IO (openFile, IOMode(ReadMode), hGetContents, Handle)
import System.IO.Error (isDoesNotExistError)

--- file handling

data UniqueFileID = UniqueFileID DeviceID FileID
uniqueFileID :: F.FileStatus -> UniqueFileID
uniqueFileID = liftM2 UniqueFileID F.deviceID F.fileID

data UniqueAccessID = UniqueAccessID UniqueFileID POSIXTime | CLInput Int
uniqueAccessID :: F.FileStatus -> UniqueAccessID
uniqueAccessID = liftM2 UniqueAccessID uniqueFileID F.modificationTimeHiRes

--- scope representation

type ID = String
data Node = Def ID P.SigmaToken | Group
data Label = Qualified ID | Single ID ID
data Precedence = Down | Up | Shadow deriving (Eq)

data Edge = Edge { label :: Label
                 , precedence :: Precedence
                 , to :: Int }

data Graph = Graph { nodes :: M.Map Int (Node, [Edge])
                   , resources :: M.Map ResourceID (Int, FilePath)
                   , asof :: POSIXTime
                   , base :: FilePath }

--- errors

data ReadError = LocateError FilePath
               | ParseError String
               | IncompleteError String
               | ImportError FilePath [ID]
               | InconsistencyError FilePath
               deriving (Show, Typeable)

instance E.Exception ReadError where
  displayException (LocateError p) = "Couldn't locate " ++ p
  displayException (ParseError e) = e
  displayException (IncompleteError e) = e
  displayException (ImportError p xs) =
    "Couldn't import " ++ intercalate ", " xs ++ " from " ++ p
  displayException (InconsistencyError p) =
    "File " ++ p ++ " changed whilst loading"

--- graph manipulation

edgesFrom :: Graph -> Int -> [Edge]
edgesFrom g n = maybe [] snd . M.lookup n $ nodes g

edgesBy :: Precedence -> Graph -> Int -> [Edge]
edgesBy p g f = filter (\e -> precedence e == p) $ edgesFrom g f

stack :: Graph -> Int -> Int -> (Graph, Int)
stack g prefer backup =
  let (g',n) = insertNode g Group
  in (,n) $ addEdges g' n [ Edge (Qualified "") Down prefer
                          , Edge (Qualified "") Shadow backup ]

insertNode :: Graph -> Node -> (Graph, Int)
insertNode g x = let n = maximum . M.keys $ nodes g
               in (g {nodes = M.insert n (x,[]) $ nodes g}, n)

addEdge :: Graph -> Int -> Edge -> Graph
addEdge g f e = g {nodes = M.adjust (fmap (e:)) f $ nodes g}

addEdges :: Graph -> Int -> [Edge] -> Graph
addEdges g f = foldr (\e g' -> addEdge g' f e) g

--- resources

data ResourceID = File DeviceID FileID POSIXTime
                | Raw Int deriving (Eq, Ord)

data Resource = Resource FilePath ResourceID String

resourceID :: Handle -> IO ResourceID
resourceID h = fmap go $ handleToFd h >>= F.getFdStatus
  where go s = let d = F.deviceID s
                   f = F.fileID s
                   t = F.modificationTimeHiRes s
               in File d f t

fileResource :: FilePath -> IO Resource
fileResource f = do fp <- openFile f ReadMode
                    liftM2 (Resource f) (resourceID fp) (hGetContents fp)

tryResources :: FilePath -> [FilePath] -> IO Resource
tryResources f [] = E.throwIO $ LocateError f
tryResources f (g:gs) = E.catchJust (guard . isDoesNotExistError)
                                    (fileResource g)
                                    (const $ tryResources f gs)

resolvePath :: FilePath -> FilePath -> IO FilePath
resolvePath based f = makeRelative based <$> makeAbsolute f

locateResource :: FilePath -> FilePath -> IO (FilePath, Resource)
locateResource based f =
  do f0 <- resolvePath based f
     Resource f' r c <- tryResources f0 [f, f <.> "sig"]
     f'' <- resolvePath based f'
     let (d,_) = splitFileName f'
     return (d, Resource f'' r c)

rawResource :: String -> Resource
rawResource s = let h = hash s
                    f = take 12 $ showHex h "input-"
                in Resource f (Raw h) s

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
  in case P.parseResult $ parse P.terms "<file>" c of
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
      g3 = addEdge g2 n (Edge (Qualified "") Up m)
      g4 = g3 {resources = M.insert r (m,f) $ resources g3}
  in (g4,m,n)

-- properly handle changing directory for relative imports, including
-- if we choose to allow for library search paths in the future
getInheritance :: Graph -> FilePath -> IO (Graph, Int)
getInheritance g fp = do (d,r) <- locateResource (base g) fp
                         withCurrentDirectory d $ fetchResource g r

--- term graphing

applyTerms :: (Graph,Int,Int) -> [P.Term] -> IO Graph
applyTerms g@(_,m,n) (t:ts) = (,m,n) <$> applyTerm g t
                                     >>= flip applyTerms ts

applyTerm :: (Graph,Int,Int) -> P.Term -> IO Graph
applyTerm (g,_,n) (P.InheritAll fp pre) =
  do (g',l) <- getInheritance g fp
     return $ addEdge g' n $ Edge (Qualified pre) Down l
applyTerm (g,_,n) (P.InheritSome fp xs) =
  do (g',l) <- getInheritance g fp
     let xs' = map (\(x,y) -> Edge (Single x y) Down l) xs
     return $ addEdges g' n xs'
applyTerm (g,m,n) P.BequeathAll =
  return $ addEdge g m (Edge (Qualified "") Down n)
applyTerm (g,m,n) (P.BequeathSome xs) =
  return $ addEdges g m $ map (\(x,y) -> Edge (Single y x) Down n) xs
applyTerm (g,_,n) (P.Group ts) =
  let (g',n') = insertNode g Group
      g'' = addEdge g' n' (Edge (Qualified "") Up n)
  in applyTerms (g'',n,n') ts
applyTerm (g,_,n) (P.LocalDef x z) =
  let (g',n') = insertNode g (Def x z)
      g'' = addEdge g' n (Edge (Single x x) Down n')
  in return $ addEdge g'' n' (Edge (Qualified "") Up n)
applyTerm (g,m,n) (P.BequeathDef (x,y) z) =
  do g' <- applyTerm (g,m,n) (P.LocalDef x z)
     return $ addEdge g' m (Edge (Single y x) Down n)
