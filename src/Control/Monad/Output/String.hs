-- | Output functions for 'String'.
--
-- Wrapping functions from 'System.IO'.

module Control.Monad.Output.String
 ( print
 , putChar
 , putStr
 , putStrLn
 , writeFile
 , appendFile
 , hPrint
 , hPutChar
 , hPutStr
 , hPutStrLn

 , module Control.Monad.Output
 )
where

import Control.Monad.Output
import Control.Monad.Output.Internal

import           Prelude hiding (print, putChar, putStr, putStrLn, writeFile, appendFile)
import           System.IO (Handle)
import qualified System.IO as IO


print :: Show a => a -> Output ()
print = Output . IO.print

putChar :: Char -> Output ()
putChar = Output . IO.putChar

putStr :: String -> Output ()
putStr = Output . IO.putStr

putStrLn :: String -> Output ()
putStrLn = Output . IO.putStrLn

writeFile :: FilePath -> String -> Output ()
writeFile path = Output . IO.writeFile path

appendFile :: FilePath -> String -> Output ()
appendFile path = Output . IO.appendFile path

hPrint :: Show a => Handle -> a -> Output ()
hPrint h = Output . IO.hPrint h

hPutChar :: Handle -> Char -> Output ()
hPutChar h = Output . IO.hPutChar h

hPutStr :: Handle -> String -> Output ()
hPutStr h = Output . IO.hPutStr h

hPutStrLn :: Handle -> String -> Output ()
hPutStrLn h = Output . IO.hPutStrLn h
