{-# LANGUAGE TupleSections #-}

module Graph
( module Graph
) where

import Common (ID)
import Resource (ResourceID)
import qualified Parser as P

import Control.Monad (guard, ap)
import Data.Maybe (mapMaybe)
import Data.List (sort, stripPrefix)
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import System.Directory (getCurrentDirectory)

--- definitions

data Node = Def ID P.SigmaToken | Group deriving (Show)
data Label = Qualified ID | Single ID ID deriving (Eq, Show)
data Precedence = Local | Neighbourhood | Shadow deriving (Eq, Ord, Show)

data Edge = Edge { label :: Label
                 , precedence :: Precedence
                 , to :: Int } deriving (Eq, Show)

instance Ord Edge where
  Edge _ p _ `compare` Edge _ q _ = p `compare` q

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
  in (,n) $ addEdges g' n [ Edge (Qualified "") Local prefer
                          , Edge (Qualified "") Shadow backup ]

insertNode :: Graph -> Node -> (Graph, Int)
insertNode g x = let n = (+1) . maximum . M.keys $ nodes g
                 in (g {nodes = M.insert n (x,[]) $ nodes g}, n)

addEdge :: Graph -> Int -> Edge -> Graph
addEdge g f e = g {nodes = M.adjust (fmap (e:)) f $ nodes g}

addEdges :: Graph -> Int -> [Edge] -> Graph
addEdges g f = foldr (\e g' -> addEdge g' f e) g

--- location

overroot :: Graph -> Graph
overroot g = g { root = overture g }

reroot :: Graph -> Int -> Graph
reroot g r = g { root = r }

search :: Graph -> ID -> [(Int, P.SigmaToken)]
search = ap searchAt root

searchAt :: Graph -> Int -> ID -> [(Int, P.SigmaToken)]
searchAt g n r = reverse $ snd switch
  where
    -- because we can reach a definition/leaf node by many different
    -- names (depending on edges), we should usually always return
    -- leaves as solutions; however, if we start searching _from_
    -- a leaf then it would be incorrect to always return the leaf,
    -- so we need to do two things instead at the beginning:
    --
    --  1) check if the leaf's internal name matches r, if so
    --     then it is a local solution => return
    --
    --  2) otherwise, search upwards _without_ marking the leaf
    --     as seen so that we retain the option of matching it
    --     under an exported name
    switch = case M.lookup n $ nodes g of
               x@(Just (Def r' _, es)) ->
                 if r == r'
                   then go' (S.empty,[]) (n,r) x
                   else gos (S.empty,[]) r es
               _ -> go (S.empty,[]) (n,r)

    go h@(s,l) q@(n',_)
      | S.member q s = h
      | otherwise = go' (S.insert q s, l) q . M.lookup n' $ nodes g

    go' h _ Nothing = h
    go' h (n',r') (Just (x,es)) = gos ((collect n' x ++) <$> h) r' es

    gos h r' es = foldl go h . mapMaybe (query r') $ sort es

    collect n' (Def _ d) = [(n',d)]
    collect _ _ = []

    query r' (Edge (Qualified p) _ n') = (n',) <$> stripPrefix p r'
    query r' (Edge (Single x y) _ n') = guard (x == r') >> return (n',y)

get :: Graph -> Int -> Maybe P.SigmaToken
get g n = do { (Def _ s, _) <- M.lookup n $ nodes g ; return s }

defs :: Graph -> [(Int, P.SigmaToken)]
defs = mapMaybe f . M.toList . nodes
  where f (n, (Def _ t, _)) = Just (n, t)
        f _ = Nothing
