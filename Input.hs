{-# LANGUAGE TupleSections #-}

module Input
( module Input
) where

--- imports

import Control.Monad (liftM2, guard)
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
import System.IO (openFile, IOMode(ReadMode), hGetContents)
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
data Label = All ID | Some [(ID,ID)]
newtype Graph = Graph [(Int, Node, Label, Int)]

--- reading

data ReadError = LocateError FilePath
               | ParseError String
               | IncompleteError String
               | ImportError FilePath [ID]
               deriving (Show, Typeable)

instance E.Exception ReadError where
  displayException (LocateError p) = "Couldn't locate " ++ p
  displayException (ParseError e) = e
  displayException (IncompleteError e) = e
  displayException (ImportError p xs) =
    "Couldn't import " ++ intercalate ", " xs ++ " from " ++ p

uniqueContents :: FilePath -> IO (UniqueAccessID, String)
uniqueContents f = do fp <- openFile f ReadMode
                      fs <- handleToFd fp >>= F.getFdStatus
                      c <- hGetContents fp
                      return (uniqueAccessID fs, c)

type FileContext a = (String, UniqueAccessID, a)

loadFile :: FilePath -> FilePath -> IO (FileContext P.Term)
loadFile base f =
  do (uai, c) <- uniqueContents f
     f' <- makeRelative base <$> makeAbsolute f
     case P.parseResult $ parse P.term f' c of
       P.ParseOK t -> return (f', uai, t)
       P.ParseError e -> E.throwIO $ ParseError e
       P.ParseIncomplete e -> E.throwIO $ ParseError e

loadString :: String -> IO (FileContext P.Term)
loadString s =
  let n = hash s
      f = take 6 $ showHex n "input-"
  in case P.parseResult $ parse P.term f s of
    P.ParseOK t -> return (f, CLInput n, t)
    P.ParseError e -> E.throwIO $ ParseError e
    P.ParseIncomplete e -> E.throwIO $ IncompleteError e

trypaths :: FilePath -> FilePath -> [FilePath] -> IO (FileContext P.Term)
trypaths _ f [] = E.throwIO $ LocateError f
trypaths base f (g:gs) =
  E.catchJust (guard . isDoesNotExistError)
              (loadFile base g)
              (const $ trypaths base f gs)

fetch :: FilePath -> FilePath -> IO (FileContext P.Term)
fetch base p = let (d,f) = splitFileName p
                   r = trypaths base f [f, f <.> "sig"]
               in withCurrentDirectory d r

load :: Graph -> [FilePath] -> (Graph, M.Map FilePath Int)
load = undefined
