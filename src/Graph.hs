{-# LANGUAGE TupleSections #-}

module Graph
( module Graph
) where

import Common (ID)
import Resource (ResourceID)
import qualified Parser as P
import qualified Data.Map as M
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import System.Directory (getCurrentDirectory)

--- definitions

data Node = Def ID P.SigmaToken | Group deriving (Show)
data Label = Qualified ID | Single ID ID deriving (Show)
data Precedence = Down | Up | Shadow deriving (Eq, Show)

data Edge = Edge { label :: Label
                 , precedence :: Precedence
                 , to :: Int } deriving (Show)

data Graph = Graph { nodes :: M.Map Int (Node, [Edge])
                   , resources :: M.Map ResourceID (Int, FilePath)
                   , asof :: POSIXTime
                   , base :: FilePath
                   , overture :: Int
                   , root :: Int } deriving (Show)

empty :: IO Graph
empty = do cwd <- getCurrentDirectory
           now <- getPOSIXTime
           return Graph { nodes = M.singleton 0 (Group, [])
                        , resources = M.empty
                        , asof = now
                        , base = cwd
                        , overture = 0
                        , root = 0 }

--- manipulation

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
insertNode g x = let n = (+1) . maximum . M.keys $ nodes g
                 in (g {nodes = M.insert n (x,[]) $ nodes g}, n)

addEdge :: Graph -> Int -> Edge -> Graph
addEdge g f e = g {nodes = M.adjust (fmap (e:)) f $ nodes g}

addEdges :: Graph -> Int -> [Edge] -> Graph
addEdges g f = foldr (\e g' -> addEdge g' f e) g
