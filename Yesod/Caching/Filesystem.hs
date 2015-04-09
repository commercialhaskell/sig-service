{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Support for caching to the filesystem.

module Yesod.Caching.Filesystem
  (filesystemCaching
  ,filesystemInvalidate)
  where

import           Blaze.ByteString.Builder

import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy as L
import           Data.Conduit (($$),($=),Flush(..),await,yield,ConduitM)
import           Data.Conduit.Binary (sinkFile)
import           Data.Conduit.Blaze
import qualified Data.Text as T
import           Prelude
import           System.Directory
import           System.FilePath

import           Yesod.Core
import           Yesod.Slug

-- | With the given key run the given action, caching any content
-- builders into a file in the directory indicated by 'withCacheDir'.
filesystemCaching :: (Slug key,HasContentType content,MonadIO m)
                  => (forall a. (FilePath -> m a) -> m a)
                  -> key
                  -> m content
                  -> m TypedContent
filesystemCaching withCache' key handler =
  withCache'
    (\dir ->
       do let fp =
                dir </>
                T.unpack (toSlug key)
          exists <- liftIO (doesFileExist fp)
          if exists
             then return (TypedContent (getContentType handler)
                                       (ContentFile fp Nothing))
             else do candidate <-
                       liftM toContent handler
                     filesystemInvalidated handler fp candidate)

-- | Invalidate (i.e. delete) a cache key, in the directory indicated
-- by 'withCache'.
filesystemInvalidate :: (Slug key,MonadIO m)
                     => (forall a. (FilePath -> m a) -> m a) -> [key] -> m ()
filesystemInvalidate withCache' keys =
  withCache'
    (\dir ->
       liftIO (mapM_ (\key ->
                        removeFile
                          (dir </>
                           T.unpack (toSlug key)))
                     keys))

-- | Cache is non-existent or invalidated, time to run the user action
-- and store the result.
filesystemInvalidated :: (HasContentType content,MonadIO m)
   => m content -> FilePath -> Content -> m TypedContent
filesystemInvalidated handler fp content =
  case content of
    ContentBuilder builder mlen ->
      do liftIO (L.writeFile
                   fp
                   (maybe id
                          (L.take . fromIntegral)
                          mlen
                          (toLazyByteString builder)))
         continue
    ContentSource src ->
      do liftIO (runResourceT
                   (src $= builderToByteStringFlush $= yieldChunks $$
                    sinkFile fp))
         continue
    ContentDontEvaluate c ->
      filesystemInvalidated handler fp c
    ContentFile file Nothing ->
      do liftIO (copyFile file fp)
         continue
    _ -> continue
  where continue =
          return (TypedContent (getContentType handler)
                               content)

-- | Yield all the chunks.
yieldChunks :: ConduitM (Flush o) o (ResourceT IO) ()
yieldChunks =
  do ma <- await
     case ma of
       Nothing -> return ()
       Just Flush -> yieldChunks
       Just (Chunk a) ->
         do yield a
            yieldChunks
