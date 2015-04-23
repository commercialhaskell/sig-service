{-# LANGUAGE OverloadedStrings #-}

-- | Upload .asc signatures.

module Handler.UploadSig where

import           Control.Monad.Trans.Resource (runResourceT)
import           Crypto.Hash.SHA256
import           Data.ByteString.Base16
import qualified Data.ByteString.Char8 as Ch
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit
import qualified Data.Conduit.Binary as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Data.Time.ISO8601
import           Import
import           Network.Wai.Conduit (sourceRequestBody)
import           Sig.Defaults
import           System.Directory
import           System.FilePath

putUploadSigR :: FilePath -> FilePath -> FilePath -> Handler ()
putUploadSigR packageName packageVersion fingerprint =
  do extra <- getExtra
     req <- getRequest
     signature <-
       liftIO (runResourceT
                 (sourceRequestBody (reqWaiRequest req) $$
                  C.take (4 * 1024 * 1024)))
     date <-
       liftIO (formatISO8601 <$> getCurrentTime)
     let digest =
           encode (hash (LB.toStrict signature))
         dir = extraRepoPath extra </> signaturesDir </> packageName </>
               packageVersion
         file = dir </> fingerprint <> "_" <> date <> "_" <> Ch.unpack digest <>
                ".asc"
     liftIO (do createDirectoryIfMissing True dir
                LB.writeFile file signature)
     $logDebug ("Received signature for " <> T.pack packageName <> "-" <>
                T.pack packageVersion <> " from " <> T.pack fingerprint <> " : " <>
                T.decodeUtf8 digest)
