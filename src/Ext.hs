module Ext
( checkAndRun
, createDirectoryExt
, removeDirectoryExt
, writeFileExt
, removeFileExt
) where

import Control.Monad (when)
import System.Directory

import Util

-- Idempotency library. A suitably-appointed Ext should only do its thing once,
-- or otherwise be safe to call many times.

-- Check if it needs to run, and only then run it.
checkAndRun :: IO Bool -> IO () -> IO ()
checkAndRun checker action = do
  isDone <- checker
  when (not isDone) action

-- I could use createDirectoryIfMissing but I'm not.
createDirectoryExt :: FilePath -> IO ()
createDirectoryExt dir = checkAndRun alreadyExists (createDirectory dir)
  where alreadyExists = doesDirectoryExist dir

removeDirectoryExt :: FilePath -> IO ()
removeDirectoryExt dir = checkAndRun alreadyGone (removeDirectory dir)
  where alreadyGone = do
          exists <- doesDirectoryExist dir
          return (not exists)

-- If the file exists and contains the string already, then we're done.
-- Otheriwse, write the file.
-- Yes, this is dumb.
writeFileExt :: FilePath -> String -> IO ()
writeFileExt f s = do
  done <- existsAndContains f s
  when (not done) $ do
    writeFile f s
  where existsAndContains f s = do
          exists <- doesFileExist f
          if exists
            then do
              s' <- readFile f
              return $ s == s'
            else
              return False

removeFileExt :: FilePath -> IO ()
removeFileExt dir = checkAndRun alreadyGone (removeFile dir)
  where alreadyGone = do
          exists <- doesFileExist dir
          return (not exists)
