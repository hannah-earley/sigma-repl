{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Common
import Graph (Graph)
import Input (loadFile, loadRaw)
import Model (eval, EvalCtx(..), tozip, showz)
import qualified Model as Mod
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
                         Automatic -> run e Up >> run e Down

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

getQuantified :: IO (String, Int)
getQuantified = fmap (\case {"" -> 1; n -> read n}) <$> (go =<< getKey)
  where go [n] | '0' <= n && n <= '9' =
          fmap (n:) <$> (go =<< getKey)
        go a = return (a, "")

normkey :: String -> String
-- arrows
normkey "\ESC[D" = "h"
normkey "\ESC[B" = "j"
normkey "\ESC[A" = "k"
normkey "\ESC[C" = "l"
-- shift-arrows
normkey "\ESC[1;2D" = "a"
normkey "\ESC[1;2B" = "s"
normkey "\ESC[1;2A" = "w"
normkey "\ESC[1;2C" = "d"
-- fn-arrows
normkey "\ESCa" = "H"
normkey "\ESC[6~" = "J"
normkey "\ESC[5~" = "K"
normkey "\ESCe" = "L"
-- other
normkey "\ESC" = "q"
normkey "\EOT" = "q"
normkey k = k

moven :: Int -> Breadcrumb -> Mod.EvalState ()
moven 0 _ = return ()
moven n b = Mod.move' b >>= \case True -> moven (n-1) b
                                  False -> return ()

movend :: Breadcrumb -> Mod.EvalState ()
movend b = do z <- Mod.goend b . it <$> get
              modify $ \c -> c {it = z}

runkey :: Int -> String -> Mod.EvalState ()
runkey 0 _ = return ()
-- move by n
runkey n "h" = moven n West
runkey n "j" = moven n South
runkey n "k" = moven n North
runkey n "l" = moven n East
-- move to end
runkey _ "H" = movend West
runkey _ "J" = movend South
runkey _ "K" = movend North
runkey _ "L" = movend East
-- evaluate
runkey n "a" = Mod.replete (fromIntegral n) >> eval Up
runkey _ "w" = eval Up
runkey _ "s" = eval Down
runkey n "d" = Mod.replete (fromIntegral n) >> eval Down
--
runkey _ _ = return ()

--- evaluation

showit :: EvalCtx -> String
showit = liftM2 (flip showz hl) it context

explore :: EvalCtx -> EnvIO' ()
explore e = limit <$> get >>= \lim -> liftIO $
      putStrLn (showit e) >> withHiddenTerminalInput (explore' e lim)

explore' :: EvalCtx -> ExtendedNat -> IO ()
explore' e l =
  do (k,n) <- getQuantified
     case normkey k of
       "q" -> return ()
       k' -> runSafe e (Mod.replete l >> runkey n k') >>= flip explore' l

run :: EvalCtx -> Direction -> EnvIO' ()
run e b = void . liftIO . runSafe e $ eval b

runSafe :: EvalCtx -> Mod.EvalState a -> IO EvalCtx
runSafe e x =
  do let (r,e') = SL.runState (runExceptT x) e
     case r of
       Left (UnificationError s) -> putStrLn $
         " >>> Unification Error: " ++ s
       Left (MoveError c) -> putStrLn $
         " >>> Structural Error (" ++ show c ++ ")"
       _ -> return ()
     putStrLn $ showit e'
     return e'
