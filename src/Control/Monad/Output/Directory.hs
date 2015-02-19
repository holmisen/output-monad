-- | Directory output functions from 'System.Directory'.

module Control.Monad.Output.Directory where

import Control.Monad.Output
import Control.Monad.Output.Internal

import Prelude hiding (appendFile, putStr, putStrLn, writeFile)

import qualified System.Directory as Dir


-- | Wraps 'Dir.createDirectory'
createDirectory :: FilePath -> Output ()
createDirectory = Output . Dir.createDirectory

-- | Wraps 'Dir.createDirectoryIfMissing'
createDirectoryIfMissing :: Bool -> FilePath -> Output ()
createDirectoryIfMissing createParents = Output . Dir.createDirectoryIfMissing createParents

-- | Wraps 'Dir.removeDirectory'
removeDirectory :: FilePath -> Output ()
removeDirectory = Output . Dir.removeDirectory

-- | Wraps 'Dir.removeDirectoryRecursive'
removeDirectoryRecursive :: FilePath -> Output ()
removeDirectoryRecursive = Output . Dir.removeDirectoryRecursive

-- | Wraps 'Dir.renameDirectory'
renameDirectory :: FilePath -> FilePath -> Output ()
renameDirectory old = Output . Dir.renameDirectory old

-- | Wraps 'Dir.setCurrentDirectory'
setCurrentDirectory :: FilePath -> Output ()
setCurrentDirectory = Output . Dir.setCurrentDirectory

-- | Wraps 'Dir.removeFile'
removeFile :: FilePath -> Output ()
removeFile = Output . Dir.removeFile

-- | Wraps 'Dir.renameFile'
renameFile :: FilePath -> FilePath -> Output ()
renameFile old = Output . Dir.renameFile old

-- | Wraps 'Dir.copyFile'
copyFile :: FilePath -> FilePath -> Output ()
copyFile old = Output . Dir.copyFile old

-- | Wraps 'Dir.setPermissions'
setPermissions :: FilePath -> Dir.Permissions -> Output ()
setPermissions path = Output . Dir.setPermissions path
