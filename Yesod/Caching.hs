{-# LANGUAGE FlexibleInstances #-}

-- | Support for caching pages.

module Yesod.Caching
  (MonadCaching(..))
  where

import Control.Concurrent.MVar.Lifted
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Prelude
import Yesod.Caching.Filesystem
import Yesod.Core
import Yesod.Slug

-- | Monad which contains a cache directory to which things can be
-- read and wrote. The 'withCache' may implement a mutual exclusion on
-- this resource.
--
class (MonadIO m,MonadBaseControl IO m) => MonadCaching m where
  type Cache m
  -- Run the given action with the given caching directory.
  withCache :: (Cache m -> m a) -> m a
  -- | With the given key run the given action, caching any content
  -- builders.
  --
  -- Default implementation is 'defaultCaching'.
  caching :: (Slug key,HasContentType content,MonadCaching m)
              => key -> m content -> m TypedContent
  -- | Invalidate the keys in cache.
  invalidate :: (Slug key,MonadCaching m) => [key] -> m ()

-- | Instance for reader monads containing a 'FilePath' will use
-- 'Yesod.Caching.Filesystem'.
instance (MonadBaseControl IO m,MonadIO m) => MonadCaching (ReaderT (MVar FilePath) m) where
  type Cache (ReaderT (MVar FilePath) m) = FilePath
  withCache cont =
    do dirVar <- ask
       withMVar dirVar cont
  caching = filesystemCaching withCache
  invalidate = filesystemInvalidate withCache
