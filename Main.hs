module Main where
import Module()
import Model()
import Input()

import System.IO
import Control.Exception (bracket)

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

main :: IO ()
main = withHiddenTerminalInput $ queryPos >>= print >> main'

main' :: IO ()
main' = getKey >>= print >> main'