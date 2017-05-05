module Main where

import System.Environment (getArgs)
import System.Console.Haskeline
  ( defaultSettings, withInterrupt, getInputLine
  , outputStrLn, InputT, runInputT, handleInterrupt )
import Control.Monad (foldM)

main :: IO ()
main = getArgs >>= foldM load emptyContext
               >>= runInputT defaultSettings . withInterrupt . loop
  where
    emptyContext = ()
    load _ = putStrLn . ("Loaded " ++)

loop :: () -> InputT IO ()
loop c = handleInterrupt (loop c) $
         do cmd <- getInputLine "σ> "
            case cmd of
              Just cmd' -> outputStrLn (cmd' ++ "\n") >> loop c
              Nothing -> return ()
