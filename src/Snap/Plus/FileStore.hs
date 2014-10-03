{-# LANGUAGE OverloadedStrings #-}
module Snap.Plus.FileStore where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           Snap.Snaplet
import           Snap.Util.FileServe
import           System.Directory    (copyFile, createDirectoryIfMissing,
                                      doesFileExist, removeFile)
import           System.FilePath     (addExtension, pathSeparator,
                                      takeExtension)
import           System.Random       (randomRIO)

data FileStore = Directory FilePath Text


setupDirectoryStore :: FilePath           -- ^ Path where files should be stored.
                    -> Text               -- ^ Base url where files should be served from
                    -> Initializer b v ()
setupDirectoryStore dir url =
  do addRoutes [(T.encodeUtf8 url, serveDirectory dir)]
     liftIO $ createDirectoryIfMissing True dir

class (Functor m, MonadIO m) => HasFileStore m where
  getFileStore :: m FileStore

-- | Takes the path to a file, stores in in the FileStore, and returns
-- a URL for it. With the Directory backend, this is an absolute path
-- without a domain (as the backend serves from the same backend as
-- the application).
storeFile :: HasFileStore m => FilePath -> m Text
storeFile pth = do s <- getFileStore
                   storeFile' s pth

-- | Takes a filestore, the path to a file, and stores the file
storeFile' :: (Functor m, MonadIO m) => FileStore -> FilePath -> m Text
storeFile' store@(Directory dir basepath) old =
  do id' <- show <$> liftIO (randomRIO (10000,9999999) :: IO Double)
     let ext = takeExtension old
     let new = addExtension id' ext
     let full = dir ++ [pathSeparator] ++ new
     e <- liftIO $ doesFileExist full
     if e
       then storeFile' store old
       else (do liftIO $ copyFile old full
                return $ T.concat ["/", basepath, "/", T.pack new])

deleteFile :: (Functor m, MonadIO m) => FileStore -> Text -> m ()
deleteFile (Directory dir base) url =
  do let file = T.unpack $ T.drop (2 + T.length base) url
     let path = dir ++ [pathSeparator] ++ file
     e <- liftIO $ doesFileExist path
     when e $ liftIO $ removeFile path
