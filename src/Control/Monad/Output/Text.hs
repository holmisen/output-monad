-- | Output functions for 'Text'.
--
-- Wrapping functions from "Data.Text.IO".

module Control.Monad.Output.Text
 ( writeFile
 , appendFile
 , hPutStr
 , hPutStrLn
 , putStr
 , putStrLn

 , module Control.Monad.Output
 )
where

import Control.Monad.Output
import Control.Monad.Output.Internal

import           Data.Text (Text)
import qualified Data.Text.IO as IO

import Prelude hiding (appendFile, putStr, putStrLn, writeFile)

import System.IO (Handle)


-- | Wraps 'IO.writeFile'
writeFile :: FilePath -> Text -> Output ()
writeFile path = Output . IO.writeFile path

-- | Wraps 'IO.appendFile'
appendFile :: FilePath -> Text -> Output ()
appendFile path = Output . IO.appendFile path

-- * Operations on handles

-- | Wraps 'IO.hPutStr'
hPutStr :: Handle -> Text -> Output ()
hPutStr h = Output . IO.hPutStr h

-- | Wraps 'IO.hPutStrLn'
hPutStrLn :: Handle -> Text -> Output ()
hPutStrLn h = Output . IO.hPutStrLn h

-- * Special cases for standard output

-- | Wraps 'IO.putStr'
putStr :: Text -> Output ()
putStr = Output . IO.putStr

-- | Wraps 'IO.putStrLn'
putStrLn :: Text -> Output ()
putStrLn = Output . IO.putStrLn
