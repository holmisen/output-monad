-- | Output functions for 'Text'.
--
-- Wrapping functions from 'Data.Text.IO'.

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


writeFile :: FilePath -> Text -> Output ()
writeFile path = Output . IO.writeFile path

appendFile :: FilePath -> Text -> Output ()
appendFile path = Output . IO.appendFile path

-- * Operations on handles

hPutStr :: Handle -> Text -> Output ()
hPutStr h = Output . IO.hPutStr h

hPutStrLn :: Handle -> Text -> Output ()
hPutStrLn h = Output . IO.hPutStrLn h

-- * Special cases for standard output

putStr :: Text -> Output ()
putStr = Output . IO.putStr

putStrLn :: Text -> Output ()
putStrLn = Output . IO.putStrLn
