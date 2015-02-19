-- | Internal module. Should not be imported unless you really know
-- what you are doing.
--
-- You are /not/ supposed to inject any IO operation in Output that
-- causes non determinism (such as readFile or getLine). That would
-- defeat the purpose of this monad.
module Control.Monad.Output.Internal where

import Control.Applicative


-- | A computation that may perform output.
--
-- Since it can do I/O, it must be run in the 'IO' monad.
newtype Output a = Output { runOutput :: IO a }

instance Functor Output where
  fmap f = Output . fmap f . runOutput

instance Applicative Output where
  pure = Output . return
  f <*> x = Output (runOutput f <*> runOutput x)

instance Monad Output where
  return = pure
  m >>= k = Output (runOutput m >>= runOutput . k)
