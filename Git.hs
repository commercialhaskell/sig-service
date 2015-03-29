{-# LANGUAGE OverloadedStrings #-}

-- | Git commands.

module Git where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Conduit.Process
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude
import           System.Directory
import           System.Exit

-- | Run the given git command and arguments.
git :: (MonadIO m,MonadThrow m,MonadLogger m)
    => String                 -- ^ Name.
    -> [String]               -- ^ Arguments.
    -> m (Either String Text) -- ^ Either stderr or stdout.
git cmd args =
  do (code,err,out) <-
       liftIO (readProcessWithExitCode "git" (cmd : args) "")
     case code of
       ExitSuccess ->
         do logging "succeeded"
            return (Right (T.pack out))
       ExitFailure{} ->
         do logging ("failed: "  <> T.pack err <> T.pack out)
            return (Left (err <> out))
  where logging text =
          $logDebug ("git " <>
                     T.intercalate " "
                                   (map T.pack (cmd : args)) <>
                     ": " <>
                     text)

-- | Clone a repo.
clone :: (MonadIO m,MonadThrow m,MonadLogger m)
      => String -> FilePath -> m ()
clone url target =
  do exists <- liftIO (doesDirectoryExist target)
     unless exists
            (do result <- git "clone" [url]
                case result of
                  Left err -> error err
                  _ -> return ())
