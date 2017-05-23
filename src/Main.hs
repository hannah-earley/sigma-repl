{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Common
import Graph (Graph)
import Input (loadFile, loadRaw)
import Model (eval, EvalCtx(..), tozip, fromzip)
import Output (printx)
import Overture (overture)
import Parser ( cmd , parseResult , Command(..)
              , EvalMode(..) , ParseResult(..) )
import qualified Parser as P
import Sigma (contextualise)

import qualified Data.Map.Lazy as M
import Text.Parsec (parse)

import Control.Monad (void)
import Control.Monad.State.Strict
import qualified Control.Monad.State.Lazy as SL
import Control.Monad.Trans (lift)
import Control.Monad.Except (runExceptT)
import Control.Exception (bracket, displayException)

import System.IO
import System.Environment (getArgs)
import System.Console.Haskeline
  ( defaultSettings, withInterrupt, getInputLine
  , outputStrLn, InputT, runInputT, handleInterrupt )
import System.Console.Haskeline.MonadException (catch)

--- io helpers

withHiddenTerminalInput :: IO a -> IO a
withHiddenTerminalInput = bracket
   (do prevBuff <- hGetBuffering stdin
       prevEcho <- hGetEcho stdin

       hSetBuffering stdin NoBuffering
       hSetEcho stdin False

       return (prevBuff, prevEcho))

   (\(prevBuff, prevEcho) -> do
       hSetBuffering stdin prevBuff
       hSetEcho stdin prevEcho)

   . const

getKey :: IO String
getKey = do c <- getChar
            b <- hReady stdin
            (c:) <$> if b then getKey else return []

hl :: String -> String
hl s = "\ESC[36;1m" ++ s ++ "\ESC[0m"

--- initiation

loadHandler :: String -> Graph -> ReadError -> IO (Graph,Bool)
loadHandler p g e =
  do putStrLn $ "Error loading module" ++ p ++ ":"
     putStrLn $ unlines . map (" >>> " ++)
              . lines $ displayException e
     return (g,False)

loadFun :: String -> (Graph -> IO Graph) -> EnvIO' Bool
loadFun p f = do g <- graph <$> get
                 (g',b) <- liftIO $ catch ((,True) <$> f g)
                                          (loadHandler p g)
                 liftIO . contextualise g' $ P.SigmaSeq []
                 modify $ \e -> e {graph = g'}
                 return b

load :: FilePath -> EnvIO' Bool
load f = loadFun (' ':f) $
            \g -> loadFile g f <* putStrLn ("Loaded: " ++ f)

loadr :: String -> EnvIO' Bool
loadr s = loadFun "" $ \g -> loadRaw g s

loads :: [FilePath] -> EnvIO' ()
loads [] = return ()
loads (f:fs) = load f >>= flip when (loads fs)

main :: IO ()
main = do args <- getArgs
          base <- overture
          evalStateT
            (loads args >> runInputT defaultSettings (withInterrupt loop))
            Env { graph = base, limit = Finite 1000 }

--- environment

type EnvIO' = StateT Env IO
type EnvIO = InputT EnvIO'

data Env = Env { graph :: Graph
               , limit :: ExtendedNat}

--- main logic

loop :: EnvIO ()
loop = handleInterrupt loop $
       getInputCmd >>= \case
         Left e -> outputStrLn (e ++ "\n") >> loop
         Right Quit -> outputStrLn "bye."
         Right Noop -> loop
         Right z -> catch (lift $ switch z) handler
                      >> outputStrLn "" >> loop
  where handler (e :: ReadError) = outputStrLn $ displayException e

switch :: Command -> EnvIO' ()
switch Quit = error "bye."
switch Noop = return ()

switch (LoadFile f) = void $ load f
switch (LoadRaw s) = void $ loadr s
switch Reload = liftIO $ putStrLn "not implemented."

switch (Relimit n) = modify $ \e -> e {limit = n}
switch (Eval m s) = do Env g l <- get
                       (it',c) <- liftIO $ contextualise g s
                       let e = EvalCtx { remaining = l
                                       , context = c
                                       , assignments = M.empty
                                       , it = tozip it' }
                       case m of
                         Manual -> explore e
                         Automatic -> run e

--- input

prompt  = "σ> "
prompt' = "σ| "

getInputCmd :: EnvIO (Either String Command)
getInputCmd = getInputCmd' prompt ""

getInputCmd' :: String -> String -> EnvIO (Either String Command)
getInputCmd' pr prefix = getInputLine pr >>= \case
  Nothing -> return $ Right Quit
  Just inp ->
    let inp' = prefix ++ inp
    in case runCmd inp' of
      ParseIncomplete _ -> getInputCmd' prompt' inp'
      ParseError e -> return $ Left e
      ParseOK v -> return $ Right v

runCmd :: String -> ParseResult Command
runCmd = parseResult . parse cmd "(input)"

--- evaluation

explore :: EvalCtx -> EnvIO' ()
explore = run

run :: EvalCtx -> EnvIO' ()
run e = let (r,e') = SL.runState (runExceptT $ eval Down) e
        in liftIO $ do case r of
                         Left (UnificationError s) -> putStrLn $
                           " >>> Unification Error: " ++ s
                         Left (MoveError c) -> putStrLn $
                           " >>> Structural Error (" ++ show c ++ ")"
                         _ -> return ()
                       putStrLn $ printx (fromzip $ it e') (context e')
