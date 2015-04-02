-- | Download the .tar.gz archive.

module Handler.DownloadArchive where

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
              liftIO (callProcess
                        "tar"
                        ["czf",outTarGz,extraRepoPath extra,"--exclude",".git"])
              return (TarContent (ContentFile outTarGz Nothing)))
