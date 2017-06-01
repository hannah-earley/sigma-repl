module Common
( module Common
) where

import Data.Typeable (Typeable)
import qualified Control.Exception as E
import Numeric.Natural

--- common data definitions

type ID = String

--- reading

data ReadError = LocateError FilePath
               | ParsingError String
               | IncompleteError String
               | ReferenceError ID
               | InconsistencyError FilePath
               | OtherError String
               deriving (Show, Typeable)

instance E.Exception ReadError where
  displayException (LocateError p) = "Couldn't locate " ++ p
  displayException (ParsingError e) = e
  displayException (IncompleteError e) = e
  displayException (ReferenceError x) = "Couldn't dereference " ++ x
  displayException (InconsistencyError p) =
    "File " ++ p ++ " changed whilst loading"
  displayException (OtherError e) = e

--- evaluation

data Direction = Up | Down

data Breadcrumb = North | East | South | West
                deriving (Show)

data EvalError = IncompleteComputation
               | UnificationError String
               | MoveError Breadcrumb
               deriving (Show)

--- view patterns

initlast :: [a] -> Maybe ([a],a)
initlast = foldr go Nothing
  where go x Nothing = Just ([],x)
        go x (Just (is,l)) = Just (x:is,l)

--- extended naturals

data ExtendedNat = Finite Natural | Infinite deriving (Eq, Ord, Show)

instance Enum ExtendedNat where
  toEnum = Finite . toEnum
  fromEnum (Finite n) = fromEnum n
  fromEnum Infinite = maxBound

  succ (Finite a) = Finite (succ a)
  succ Infinite = Infinite

  pred (Finite a) = Finite (pred a)
  pred Infinite = Infinite

instance Num ExtendedNat where
  Finite a + Finite b = Finite $ a + b
  Infinite + _ = Infinite
  _ + Infinite = Infinite

  Finite a - Finite b = Finite $ a - b
  Infinite - _ = Infinite
  Finite _ - Infinite = -Infinite

  Finite a * Finite b = Finite $ a * b
  Infinite * _ = Infinite
  _ * Infinite = Infinite

  signum (Finite 0) = Finite 0
  signum _ = Finite 1

  fromInteger = Finite . fromInteger

  abs = id

  negate (Finite a) = Finite $ negate a
  negate Infinite = E.throw E.Underflow
