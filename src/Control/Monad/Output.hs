-- | The 'Output' monad is an output subset of the 'IO' monad.
--
-- = Motivation
--
-- IO functions may perform both input and output. Output causes side
-- effects while input causes non determinism. Given
--
-- > foo :: S -> IO T
--
-- we can only know that `foo` takes an `S`, returns a `T` and
-- possibly has side effects. We cannot know if it is non
-- deterministic.
--
-- But given
--
-- > bar :: S -> Output T
--
-- we know that while `bar` may have side effects, it is
-- deterministic (in the absence of exceptions).
--
-- = What about the `Writer` monad
--
-- The 'Control.Monad.Writer' monad can be used to collect output from
-- a pure function to be output to file(s) etc. But then the whole
-- computation must be completed before any data is output. In a long
-- running computation it may be desirable to interleave output with
-- processing.
--
-- = When is this useful
--
-- Logging is an obvious example, but one may also want to dump other
-- data to files during processing.

module Control.Monad.Output
 ( Output
 , runOutput
   
 -- * File handles for output

 , OutputMode (..)
 , hClose
 , hFlush
 , openFile
 , withFile
 )
where

import Control.Monad.Output.Internal

import           System.IO (Handle)
import qualified System.IO as IO


hClose :: Handle -> Output ()
hClose = Output . IO.hClose

hFlush :: Handle -> Output ()
hFlush = Output . IO.hFlush

data OutputMode = WriteMode | AppendMode deriving (Eq,Show)

toIOMode WriteMode = IO.WriteMode
toIOMode AppendMode = IO.AppendMode

openFile :: FilePath -> OutputMode -> Output Handle
openFile path = Output . IO.openFile path . toIOMode

-- TODO: Consider exceptions
withFile :: FilePath -> OutputMode -> (Handle -> Output a) -> Output a
withFile path mode f =
  Output $ IO.withFile path (toIOMode mode) (runOutput . f)
