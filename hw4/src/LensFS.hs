{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module LensFS 
       ( FS (..)
       , scanDir
       , cd
       , ls
       ) where

import Control.Lens          hiding (iso, Iso)
import System.Directory      (doesFileExist, listDirectory)
import System.FilePath       (splitPath, takeFileName, (</>), splitExtension)
import Control.Applicative   (Const (..))
import Data.Tagged           (Tagged (..))

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

lsAll :: Traversal' FS FilePath
lsAll = contents . each . con name lsAll 
  where 
    con :: Traversal' a b -> Traversal' a b -> Traversal' a b
    con a b fun obj = a fun obj *> b fun obj

renameExt :: String -> FS -> FS
renameExt ext fs = fs & contents . traversed . filtered isFile . name %~ change
  where
    change p = let (fn, _) = splitExtension p 
               in fn ++ "." ++ ext

deleteDir :: FilePath -> Getter FS FS
deleteDir fp = to (contents %~ del)
  where 
    del = filter $ \f -> not (isFile f) 
                      && f ^. name           == fp 
                      && lengthOf contents f == 0


-- ISO

type Iso b a = forall p f . (Profunctor p, Functor f) => p a (f a) -> p b (f b)

iso :: (b -> a) -> (a -> b) -> Iso b a
iso fba fab = dimap fba $ fmap fab

from :: Iso b a -> Iso a b
from is =
  let fab = unTagged $ is (Tagged id)
      fba = getConst . is Const
  in iso fab fba