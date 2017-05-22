{-# LANGUAGE LambdaCase #-}

module Main where

import Input ()
import Parser ()
import Overture ()
import Algorithm ()
import Sigma ()
import Output ()
import Model ()

import Control.Monad (foldM)
import System.Environment (getArgs)
import System.Console.Haskeline
  ( defaultSettings, withInterrupt, getInputLine
  , outputStrLn, InputT, runInputT, handleInterrupt )

main :: IO ()
main = getArgs >>= foldM load emptyContext
               >>= runInputT defaultSettings . withInterrupt . loop
  where
    emptyContext = ()
    load _ = putStrLn . ("Loaded " ++)

loop :: () -> InputT IO ()
loop c = handleInterrupt (loop c) $
           getInputLine "σ> " >>= \case
             Just cmd' -> outputStrLn (cmd' ++ "\n") >> loop c
             Nothing -> return ()
