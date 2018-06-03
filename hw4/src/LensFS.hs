{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module LensFS 
       ( FS (..)
       , scanDir
       , cd
       , ls
       ) where

import Control.Lens
import System.Directory      (doesFileExist, listDirectory)
import System.FilePath       (splitPath, takeFileName, (</>))

-- FileSystem

data FS
    = Dir
          { _name     :: FilePath  -- название папки, не полный путь
          , _contents :: [FS]
          }
    | File
          { _name     :: FilePath  -- название файла, не полный путь
          }


isFile :: FS -> Bool
isFile (File _) = True
isFile _        = False

isDir :: FS -> Bool
isDir (Dir _ _) = True
isDir _         = False

scanDir :: FilePath -> IO FS
scanDir path = do
    isFile' <- doesFileExist path
    if isFile'
    then pure $ File $ takeFileName path
    else do
          content <- listDirectory path
          Dir (last $ splitPath path) <$> mapM (scanDir . (</>) path) content

makeLenses ''FS
makePrisms ''FS

cd :: FilePath -> Traversal' FS FS
cd dir = contents . traversed . filtered (\x -> isDir x && x ^. name == dir)

ls :: Traversal' FS FS
ls = contents . each

file :: FilePath -> Traversal' FS FS
file f = contents . traversed . filtered (\x -> isFile x && x ^. name == f)