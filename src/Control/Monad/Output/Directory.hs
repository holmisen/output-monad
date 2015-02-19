-- | Directory output functions wrapping 'System.Directory'.

module Control.Monad.Output.Directory where

import Control.Monad.Output
import Control.Monad.Output.Internal

import Prelude hiding (appendFile, putStr, putStrLn, writeFile)

import qualified System.Directory as Dir


createDirectory :: FilePath -> Output ()
createDirectory = Output . Dir.createDirectory

createDirectoryIfMissing :: Bool -> FilePath -> Output ()
createDirectoryIfMissing createParents = Output . Dir.createDirectoryIfMissing createParents

removeDirectory :: FilePath -> Output ()
removeDirectory = Output . Dir.removeDirectory

removeDirectoryRecursive :: FilePath -> Output ()
removeDirectoryRecursive = Output . Dir.removeDirectoryRecursive

renameDirectory :: FilePath -> FilePath -> Output ()
renameDirectory old = Output . Dir.renameDirectory old

setCurrentDirectory :: FilePath -> Output ()
setCurrentDirectory = Output . Dir.setCurrentDirectory

removeFile :: FilePath -> Output ()
removeFile = Output . Dir.removeFile

renameFile :: FilePath -> FilePath -> Output ()
renameFile old = Output . Dir.renameFile old

copyFile :: FilePath -> FilePath -> Output ()
copyFile old = Output . Dir.copyFile old

setPermissions :: FilePath -> Dir.Permissions -> Output ()
setPermissions path = Output . Dir.setPermissions path
