-- | Download the .tar.gz archive.

module Handler.DownloadArchive where

import Import
import System.Directory
import System.Process

-- | Download a .tar.gz of the archive.
getDownloadArchiveR :: Handler TypedContent
getDownloadArchiveR =
  do extra <- getExtra
     tmp <- liftIO getTemporaryDirectory
     let outTarGz = tmp ++ "/sig-archive.tar.gz"
     liftIO (callProcess "tar"
                         ["czf",outTarGz,extraRepoPath extra,"--exclude",".git"])
     return (TypedContent "application/x-tar"
                          (ContentFile outTarGz Nothing))
