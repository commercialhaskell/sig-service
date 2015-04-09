{-# LANGUAGE OverloadedStrings #-}

-- | Upload .asc signatures.

module Handler.UploadSig where

import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Lazy as LB (toStrict)
import           Data.Conduit (($$))
import qualified Data.Conduit.Binary as C (take)
import           Data.String (fromString)
import           Import
import           Network.Wai.Conduit (sourceRequestBody)
import           Sig.Types (Signature(..))

putUploadSigR :: Text -> Text -> Text -> Handler ()
putUploadSigR packageName packageVersion fingerprint =
  do req <- getRequest
     signature <-
       liftIO (fmap (Signature . LB.toStrict)
                    (runResourceT
                       (sourceRequestBody (reqWaiRequest req) $$
                        C.take (1024 * 1024))))
     $logDebug ("Received signature for " <> packageName <> "-" <>
                packageVersion <> " from " <> fingerprint <> ": " <>
                fromString (show signature))
