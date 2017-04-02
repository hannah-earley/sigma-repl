{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Main where
import Module
import Model
import Input

import Data.Tuple (swap)
import qualified Data.Map.Lazy as M
import Control.Monad
import Control.Monad.IO.Class
import System.IO
import System.Environment (getArgs)
import Control.Exception (bracket, displayException, catch, throwIO)
import System.Console.Haskeline (defaultSettings, getInputLine, outputStrLn,
                              InputT, runInputT, withInterrupt, handleInterrupt)

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

queryPos :: IO String
queryPos = putStr "\ESC[6n\n\ESC[A" >> getKey

hl :: String -> String
hl s = "\ESC[36;1m" ++ s ++ "\ESC[0m"

--- initiation

main :: IO ()
main = let argx = getArgs >>= foldM f emptyContext >>= dumpl
       in catch argx handler >>= runInputT defaultSettings . withInterrupt . begin
  where
    f c p = loadprog p >>= either (throwIO . OtherError) return . incompile c
    begin ctx = loop $ Env {ctx, lim = Just 1000}

    handler e =
      do putStrLn "Error in loading CLI modules:"
         putStrLn $ " >>> " ++ displayException (e :: InterpreterException)
         putStrLn "Loaded: none\n"
         return emptyContext

    dumpl c = c <$ let loaded = hl . unwords . M.keys $ exposed c
                   in if null loaded then return ()
                      else putStrLn ("Loaded: " ++ loaded ++ "\n")

--- environment

data Env = Env { ctx :: Context (Expr ID) ID
               , lim :: Maybe Int }

evalx :: Env -> Direction -> Expression -> Expression
evalx (Env {ctx, lim = Just n}) = eval n ctx
evalx (Env {ctx, lim = Nothing}) = exec ctx

--- main logic

loop :: Env -> InputT IO ()
loop ctx = handleInterrupt (loop ctx) $
           do cmd <- getInputCmd
              case cmd of
                Right Quit -> outputStrLn "bye."
                Right Noop -> loop ctx
                Left e -> outputStrLn (" [exception:] " ++
                            displayException e ++ "\n") >> loop ctx
                Right z -> liftIO (switch ctx z) <* outputStrLn "" >>= loop

switch :: Env -> Command -> IO Env
switch _ Quit = error "bye."
switch c Noop = return c
switch c (Relimit (Left ())) = c <$ putStrLn (" current eval limit: " ++ show (lim c))
switch c (Relimit (Right lim')) = return $ c {lim = lim'}
switch c (Load g) = case incompile (ctx c) g of
                      Left e -> c <$ putStrLn (" [error:] " ++ e)
                      Right c' -> c {ctx = c'} <$ dumpdiff (ctx c) c'

switch c (Eval mode e) = c <$ either (putStrLn . (" [error:] " ++)) (gox mode)
                                     (contextualise (ctx c) e)
  where
    gox Manual x = liftIO $ explore c x
    gox Automatic x = do putStrLn . show . evalx c Up $ x
                         putStrLn . show . evalx c Down $ x
switch c@(Env {ctx}) Info = c <$ dumpsyms ctx (doppels ctx)

--- symbol table

dumpsyms :: Context (Expr ID) ID -> [[(ID, Int)]] -> IO ()
dumpsyms ctx = putStr . unlines . concatMap (infos . map info)
  where
    info r = let entry = infor m r ++ " = " ++ (maybe "!?!?!" (show . reslot (infor m')) . M.lookup r $ symbols ctx)
             in if r `elem` exposed ctx then hl ("`" ++ entry) else ("  " ++ entry)

      -- (if r `elem` exposed ctx then "\ESC[32;1m`" else "  ") ++
      --            infor m r ++ " = " ++
      --            (maybe "!?!?!" (show . reslot (infor m')) . M.lookup r $ symbols ctx)

    m = M.fromListWith (\x y -> x ++ ", " ++ y) . map swap . M.toList $ exposed ctx
    m' = M.fromListWith const . map swap . M.toList $ exposed ctx

    infor mm r@(s,n) = M.findWithDefault ("\ESC[D" ++ show n ++ ":" ++ s) r mm

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

-- assume l == r
data ExprOHC l r = HAs l | HSeq [Expr l r] [Expr l r]
data ExprZip r = ExprZip (Context (Expr r) r) (Expr r (r, Int)) [ExprOHC r (r, Int)]
data Breadcrumb = North | East | South | West

tozip :: Context (Expr r) r -> Expr r (r, Int) -> ExprZip r
tozip c = flip (ExprZip c) []

fromzip :: Ord r => ExprZip r -> Expr r (r, Int)
fromzip = (\(ExprZip _ e []) -> e) . goend North

go :: Ord r => Breadcrumb -> ExprZip r -> ExprZip r
go d = snd . go' d

goend :: Ord r => Breadcrumb -> ExprZip r -> ExprZip r
goend d = snd . until (not . fst) (go' d . snd) . (True,)

go' :: Ord r => Breadcrumb -> ExprZip r -> (Bool, ExprZip r)
go' North (ExprZip c e (HAs l : zs)) = (True, ExprZip c (As l e) zs)
go' North (ExprZip c e (HSeq l r : zs)) = (True, ExprZip c (Seq $ foldl (flip (:)) (e:r) l) zs)
go' East (ExprZip c l (HSeq ls (r:rs) : zs)) = (True, ExprZip c r (HSeq (l:ls) rs : zs))
go' West (ExprZip c r (HSeq (l:ls) rs : zs)) = (True, ExprZip c l (HSeq ls (r:rs) : zs))
go' South (ExprZip c (Seq (e:es)) zs) = (True, ExprZip c e (HSeq [] es : zs))
go' South (ExprZip c (As l e) zs) = (True, ExprZip c e (HAs l : zs))
-- go' South z@(ExprZip c (Ref r) zs) = case M.lookup r (symbols c) of
--                                         Nothing -> (False, z)
--                                         Just e -> (True, ExprZip c e (HAs (fst r) : zs))
go' _ z = (False, z)

gorun :: (Context (Expr r) r -> Expr r (r, Int) -> Expr r (r, Int)) -> ExprZip r -> ExprZip r
gorun f (ExprZip c e z) = ExprZip c (f c e) z

instance Show (ExprZip String) where
  show (ExprZip c e z) = show . fromzip $ ExprZip c (Label . hl $ show e) z

explore :: Env -> Expression -> IO ()
explore e = withHiddenTerminalInput . explore' (lim e) . tozip (ctx e)

exploread :: IO (Int, String)
exploread = do k <- getKey
               (n,v) <- exploread' (0,k)
               return (if n <= 0 then 1 else n, v)

exploread' :: (Int, String) -> IO (Int, String)
exploread' (m, [n]) | '0' <= n && n <= '9' =
    (10*m + read [n],) <$> getKey >>= exploread'
exploread' z = return $ interpkey <$> z

interpkey :: String -> String
-- arrows
interpkey "\ESC[D" = "h"
interpkey "\ESC[B" = "j"
interpkey "\ESC[A" = "k"
interpkey "\ESC[C" = "l"
-- shift-arrows
interpkey "\ESC[1;2D" = "a"
interpkey "\ESC[1;2B" = "s"
interpkey "\ESC[1;2A" = "w"
interpkey "\ESC[1;2C" = "d"
-- fn-arrows
interpkey "\ESCa" = "H"
interpkey "\ESC[6~" = "J"
interpkey "\ESC[5~" = "K"
interpkey "\ESCe" = "L"
-- other
interpkey "q" = "\ESC"
interpkey "\EOT" = "\ESC"
interpkey s = s

gon n d z = iterate (go d) z !! n

explore' :: Maybe Int -> ExprZip ID -> IO ()
explore' m z = do print z
                  (n, k) <- exploread
                  let f = case m of
                            Nothing -> flip exec
                            Just n' -> flip (eval n')
                  case k of
                    "h" -> explore' m $ gon n West z
                    "j" -> explore' m $ gon n South z
                    "k" -> explore' m $ gon n North z
                    "l" -> explore' m $ gon n East z

                    "H" -> explore' m $ goend West z
                    "J" -> explore' m $ goend South z
                    "K" -> explore' m $ goend North z
                    "L" -> explore' m $ goend East z

                    "a" -> explore' m $ gorun (flip (eval n) Up) z
                    "w" -> explore' m $ gorun (f Up) z
                    "s" -> explore' m $ gorun (f Down) z
                    "d" -> explore' m $ gorun (flip (eval n) Down) z

                    "\ESC" -> return ()
  
                    _ -> explore' m z
  -- print >> explore' e x

--- command reading

prompt  = "σ> "
prompt' = "σ| "

getInputCmd :: InputT IO (Either InterpreterException Command)
getInputCmd = getInputCmd' prompt ""

getInputCmd' :: String -> String -> InputT IO (Either InterpreterException Command)
getInputCmd' pr prefix = do inp <- ((prefix ++) <$>) <$> getInputLine pr
                            case inp of
                              Nothing -> return $ Right Quit
                              Just inp' ->
                                do cmd <- liftIO $ runcmd inp'
                                   case cmd of
                                     Left IncompleteParseError -> getInputCmd' prompt' inp'
                                     _ -> return cmd