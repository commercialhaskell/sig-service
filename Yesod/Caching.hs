{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Support for caching pages.

module Yesod.Caching
  (MonadCaching(..)
  ,invalidate
  ,defaultCaching)
  where

import           Blaze.ByteString.Builder
import           Control.Concurrent.MVar.Lifted
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
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

-- | Monad which contains a cache directory to which things can be
-- read and wrote. The 'withCacheDir' may implement a mutual exclusion
-- on this resource.
--
class (MonadIO m,MonadBaseControl IO m) => MonadCaching m where
  -- Run the given action with the given caching directory.
  withCacheDir :: (FilePath -> m a) -> m a
  {-# MINIMAL withCacheDir #-}
  -- | With the given key run the given action, caching any content
  -- builders.
  --
  -- Default implementation is 'defaultCaching'.
  caching :: (Slug key,HasContentType content,MonadCaching m)
          => key -> m content -> m TypedContent
  caching = defaultCaching

instance (MonadBaseControl IO m,MonadIO m) => MonadCaching (ReaderT (MVar FilePath) m) where
  withCacheDir cont =
    do dirVar <- ask
       withMVar dirVar cont

-- | With the given key run the given action, caching any content
-- builders.
defaultCaching :: (Slug key,HasContentType content,MonadCaching m)
               => key -> m content -> m TypedContent
defaultCaching key handler =
  withCacheDir
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
                     invalidated handler fp candidate)

-- | Invalidate (i.e. delete) a cache key.
invalidate :: (Slug key,MonadCaching m) => [key] -> m ()
invalidate keys =
  withCacheDir
    (\dir ->
       liftIO (mapM_ (\key ->
                        removeFile
                          (dir </>
                           T.unpack (toSlug key)))
                     keys))

-- | Cache is non-existent or invalidated, time to run the user action
-- and store the result.
invalidated :: (HasContentType content,MonadIO m)
   => m content -> FilePath -> Content -> m TypedContent
invalidated handler fp content =
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
      invalidated handler fp c
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
