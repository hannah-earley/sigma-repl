{-# LANGUAGE NamedFieldPuns #-}

module Main where
import Module
import Model
import Input

import Data.Tuple (swap)
import qualified Data.Map.Lazy as M
import System.IO
import System.Environment (getArgs)
import Control.Exception (bracket, bracketOnError, displayException, catch, throwIO)
import Control.Monad
import System.Console.Haskeline (defaultSettings, getInputLine)
import System.Console.Haskeline.IO (initializeInput, cancelInput, closeInput, queryInput, InputState)

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

withHaskeline :: (InputState -> IO a) -> IO a
withHaskeline go = bracketOnError
                     (initializeInput defaultSettings)
                     cancelInput
                     (\hd -> go hd <* closeInput hd)

getKey :: IO String
getKey = do c <- getChar
            b <- hReady stdin
            (c:) <$> if b then getKey else return []

queryPos :: IO String
queryPos = putStr "\ESC[6n\n\ESC[A" >> getKey

--- initiation

main :: IO ()
main = let argx = getArgs >>= foldM go emptyContext >>= dumpl
       in catch argx handler >>= withHaskeline . loop'
  where
    go c p = loadprog p >>= either (throwIO . OtherError) return . incompile c
    loop' ctx = loop $ Env {ctx, lim = Just 1000}

    handler :: InterpreterException -> IO (Context (Expr ID) ID)
    handler e = do putStrLn "Error in loading CLI modules:"
                   putStrLn $ " >>> " ++ displayException e
                   putStrLn "Loaded: none\n"
                   return emptyContext

    dumpl c = c <$ let loaded = unwords (M.keys $ exposed c)
                   in if null loaded then return ()
                      else putStrLn ("Loaded: " ++ loaded ++ "\n")

--- environment

data Env = Env { ctx :: Context (Expr ID) ID
               , lim :: Maybe Int }

evalx :: Env -> Direction -> Expression -> Expression
evalx (Env {ctx, lim = Just n}) = eval n ctx
evalx (Env {ctx, lim = Nothing}) = exec ctx

--- main logic

loop :: Env -> InputState -> IO ()
loop ctx hd = do cmd <- getInputCmd hd
                 case cmd of
                   Right Quit -> putStrLn "bye."
                   Right Noop -> loop ctx hd
                   Left e -> putStrLn (" [exception:] " ++ displayException e ++ "\n") >> loop ctx hd
                   Right z -> goc ctx z <* putStrLn "" >>= flip loop hd

goc :: Env -> Command -> IO Env
goc _ Quit = error "bye."
goc c Noop = return c
goc c (Relimit (Left ())) = c <$ putStrLn (" current eval limit: " ++ show (lim c))
goc c (Relimit (Right lim')) = return $ c {lim = lim'}
goc c (Load g) = case incompile (ctx c) g of
                   Left e -> c <$ putStrLn (" [error:] " ++ e)
                   Right c' -> c {ctx = c'} <$ dumpdiff (ctx c) c'

goc c (Eval mode e) = c <$ either (putStrLn . (" [error:] " ++)) (gox mode)
                                  (contextualise (ctx c) e)
  where
    gox Manual x = explore c x
    gox Automatic x = do putStrLn . show . evalx c Up $ x
                         putStrLn . show . evalx c Down $ x
goc c@(Env {ctx}) Info = c <$ dumpsyms ctx (doppels ctx)

--- symbol table

dumpsyms :: Context (Expr ID) ID -> [[(ID, Int)]] -> IO ()
dumpsyms ctx = putStr . unlines . concatMap (infos . map info)
  where
    info r = (if r `elem` exposed ctx then "*" else " ") ++
                 infor m r ++ " = " ++
                 (maybe "!?!?!" (show . reslot (infor m')) . M.lookup r $ symbols ctx)

    m = M.fromListWith (\x y -> x ++ ", " ++ y) . map swap . M.toList $ exposed ctx
    m' = M.fromListWith const . map swap . M.toList $ exposed ctx

    infor mm r@(s,n) = M.findWithDefault (show n ++ ":" ++ s) r mm

    infos [] = []
    infos [r] = [" " ++ r]
    infos (r:rs) = ("\x250c" ++ r) : infos' rs

    infos' [] = []
    infos' [r] = ["\x2514" ++ r]
    infos' (r:rs) = ("\x2502" ++ r) : infos' rs

expdiff :: Context (Expr ID) ID -> Context (Expr ID) ID -> [[(ID,Int)]]
expdiff c c' = let dopps = concat $ doppels c
                   dopps' = doppels c'
               in map (filter (`notElem` dopps)) dopps'

dumpdiff :: Context (Expr ID) ID -> Context (Expr ID) ID -> IO ()
dumpdiff c c' = dumpsyms c' $ expdiff c c'

--- manual mode

explore :: Env -> Expression -> IO ()
explore e x = withHiddenTerminalInput $ explore' e x

explore' :: Env -> Expression -> IO ()
explore' e x = getKey >>= print >> explore' e x

--- command reading

prompt  = "σ> "
prompt' = "σ| "

getInputCmd :: InputState -> IO (Either InterpreterException Command)
getInputCmd = getInputCmd' prompt ""

getInputCmd' :: String -> String -> InputState -> IO (Either InterpreterException Command)
getInputCmd' pr prefix hd = do inp <- ((prefix ++) <$>) <$> queryInput hd (getInputLine pr)
                               case inp of
                                 Nothing -> return $ Right Quit
                                 Just inp' ->
                                   do cmd <- runcmd inp'
                                      case cmd of
                                        Left IncompleteParseError -> getInputCmd' prompt' inp' hd
                                        _ -> return cmd