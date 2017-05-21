module Common
( module Common
) where

import Data.Typeable (Typeable)
import qualified Control.Exception as E
import Data.List (intercalate)

type ID = String

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
