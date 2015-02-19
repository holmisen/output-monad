-- | Output functions for 'String'.
--
-- Wrapping functions from "System.IO".

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


-- | Wraps 'IO.print'
print :: Show a => a -> Output ()
print = Output . IO.print

-- | Wraps 'IO.putChar'
putChar :: Char -> Output ()
putChar = Output . IO.putChar

-- | Wraps 'IO.putStr'
putStr :: String -> Output ()
putStr = Output . IO.putStr

-- | Wraps 'IO.putStrLn'
putStrLn :: String -> Output ()
putStrLn = Output . IO.putStrLn

-- | Wraps 'IO.writeFile'
writeFile :: FilePath -> String -> Output ()
writeFile path = Output . IO.writeFile path

-- | Wraps 'IO.appendFile'
appendFile :: FilePath -> String -> Output ()
appendFile path = Output . IO.appendFile path

-- | Wraps 'IO.hPrint'
hPrint :: Show a => Handle -> a -> Output ()
hPrint h = Output . IO.hPrint h

-- | Wraps 'IO.hPutChar'
hPutChar :: Handle -> Char -> Output ()
hPutChar h = Output . IO.hPutChar h

-- | Wraps 'IO.hPutStr'
hPutStr :: Handle -> String -> Output ()
hPutStr h = Output . IO.hPutStr h

-- | Wraps 'IO.hPutStrLn'
hPutStrLn :: Handle -> String -> Output ()
hPutStrLn h = Output . IO.hPutStrLn h
