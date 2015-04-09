{-# LANGUAGE OverloadedStrings #-}

-- | Download the .tar.gz archive.

module Handler.DownloadArchive where

import Control.Monad
import Import
import System.Directory
import System.Process
import Yesod.Caching

-- | Download a .tar.gz of the archive.
getDownloadArchiveR :: Handler TypedContent
getDownloadArchiveR =
  caching ArchiveDownloadC
          (do extra <- getExtra
              tmp <- liftIO getTemporaryDirectory
              let outTarGz = tmp ++ "/sig-archive.tar.gz"
              void (liftIO (fmap waitForProcess
                                 (runProcess
                                    "git"
                                    ["archive"
                                    ,"--prefix=sig-archive/"
                                    ,"--format=tar.gz"
                                    ,"--output=" <> outTarGz
                                    ,"HEAD"]
                                    (Just $ extraRepoPath extra)
                                    Nothing
                                    Nothing
                                    Nothing
                                    Nothing)))
              return (TarContent (ContentFile outTarGz Nothing)))
