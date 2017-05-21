module Resource
( module Resource
) where

import Common (ReadError(LocateError))

import Control.Monad (guard, ap)
import qualified Control.Exception as E
import Numeric (showHex)
import Data.Hashable (hash)
import Data.Time.Clock.POSIX (POSIXTime)

import qualified System.Posix.Files as F
import System.Posix.Types (DeviceID, FileID, Fd)
import System.Posix.IO (handleToFd, fdToHandle)
import System.Directory (canonicalizePath)
import System.FilePath.Posix ( splitFileName
                             , splitPath
                             , joinPath
                             , (<.>))
import System.IO (openFile, IOMode(ReadMode), hGetContents)
import System.IO.Error (isDoesNotExistError)

---

data ResourceID = File DeviceID FileID POSIXTime
                | Raw Int deriving (Eq, Ord, Show)

data Resource = Resource FilePath ResourceID String
              deriving (Show)

resourceID :: Fd -> IO ResourceID
resourceID fd = go <$> F.getFdStatus fd
  where go s = let d = F.deviceID s
                   f = F.fileID s
                   t = F.modificationTimeHiRes s
               in File d f t

fileResource :: FilePath -> IO Resource
fileResource f = do fd <- openFile f ReadMode >>= handleToFd
                    ap (Resource f <$> resourceID fd)
                       (fdToHandle fd >>= hGetContents)

tryResources :: FilePath -> [FilePath] -> IO Resource
tryResources f [] = E.throwIO $ LocateError f
tryResources f (g:gs) = E.catchJust (guard . isDoesNotExistError)
                                    (fileResource g)
                                    (const $ tryResources f gs)

-- takes two split **canonical** paths, eliminates their common
-- prefix and finds the way from the first to the second via ..
-- N.B. this requires canonical paths such that symlinks have
-- been eliminated, as otherwise the new path may not be strictly
-- equivalent (which is why System.FilePath.Posix.makeRelative
-- does not do the below behaviour)
makeCanRelative :: [FilePath] -> [FilePath] -> [FilePath]
makeCanRelative [] [] = []
makeCanRelative xs [] = map (const "../") xs
makeCanRelative [] ys = ys
makeCanRelative (x:xs) (y:ys)
  | x == y = makeCanRelative xs ys
  | otherwise = "../" : makeCanRelative xs (y:ys)

resolvePath :: FilePath -> FilePath -> IO FilePath
resolvePath based f =
  do bc <- splitPath <$> canonicalizePath based
     fc <- splitPath <$> canonicalizePath f
     return . joinPath $ makeCanRelative bc fc

locateResource :: FilePath -> FilePath -> IO (FilePath, Resource)
locateResource based f =
  do f0 <- resolvePath based f
     Resource f' r c <- tryResources f0 [f, f <.> "sig"]
     f'' <- resolvePath based f'
     let (d,_) = splitFileName f'
     return (d, Resource f'' r c)

rawResource :: String -> Resource
rawResource s = let h = hash s
                    hx = take 6 $ showHex (abs h) ""
                in Resource ("input-" ++ hx) (Raw h) s
