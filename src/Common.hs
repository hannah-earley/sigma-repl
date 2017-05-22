{-# LANGUAGE LambdaCase #-}

module Common
( module Common
) where

import Data.Typeable (Typeable)
import qualified Control.Exception as E

type ID = String

data ReadError = LocateError FilePath
               | ParseError String
               | IncompleteError String
               | ReferenceError ID
               | InconsistencyError FilePath
               | OtherError String
               deriving (Show, Typeable)

instance E.Exception ReadError where
  displayException (LocateError p) = "Couldn't locate " ++ p
  displayException (ParseError e) = e
  displayException (IncompleteError e) = e
  displayException (ReferenceError x) = "Couldn't dereference " ++ x
  displayException (InconsistencyError p) =
    "File " ++ p ++ " changed whilst loading"
  displayException (OtherError e) = e

initlast :: [a] -> Maybe ([a],a)
initlast = foldr go Nothing
  where go x Nothing = Just ([],x)
        go x (Just (is,l)) = Just (x:is,l)
