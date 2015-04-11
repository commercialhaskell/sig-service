{-# LANGUAGE OverloadedStrings #-}

-- | Upload .asc signatures.

module Handler.UploadSig where

import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit
import           Data.String
import qualified Data.Conduit.Binary as C
import           Import
import           Network.Wai.Conduit (sourceRequestBody)
import           System.Directory
import           System.FilePath

putUploadSigR :: FilePath -> FilePath -> FilePath -> Handler ()
putUploadSigR packageName packageVersion fingerprint =
  do extra <- getExtra
     req <- getRequest
     let dir = extraRepoPath extra </> packageName </> packageVersion
     liftIO (do createDirectoryIfMissing True dir
                runResourceT
                  (sourceRequestBody (reqWaiRequest req) $$
                   C.sinkFile (dir </> fingerprint <> ".asc")))
     $logDebug ("Received signature for " <> fromString packageName <> "-" <>
                fromString packageVersion <> " from " <> fromString fingerprint)
