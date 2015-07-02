{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Lifted
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Catch
import           Data.Default (def)
import           Import
import           Network.HTTP.Client.Conduit (newManager)
import           Network.Wai.Logger (clockDateCacher)
import           Network.Wai.Middleware.RequestLogger ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination)
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import           System.Directory
import           System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import           System.Process
import           Yesod.Core.Types (loggerSet, Logger (Logger))
import           Yesod.Default.Config
import           Yesod.Default.Handlers
import           Yesod.Default.Main

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import           Handler.Home
import           Handler.UploadSig
import           Handler.DownloadArchive

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BC
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Conduit (Request(..))
import qualified Network.HTTP.Conduit as HC
import           Path
import qualified System.Environment as E
import           System.IO
import           System.Posix

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO (Application, LogFunc)
makeApplication conf =
  do foundation <- makeFoundation conf
     -- Initialize the logging middleware
     logWare <-
       mkRequestLogger
         def {outputFormat =
                if development
                   then Detailed True
                   else Apache FromSocket
             ,destination = RequestLogger.Logger $ loggerSet $
                                                   appLogger foundation}
     -- Create the WAI application and apply middlewares
     app <- toWaiAppPlain foundation
     let logFunc =
           messageLoggerSource foundation
                               (appLogger foundation)
     -- Git Add/Commit/Pull/Push every X minutes
     void
       (forkIO (runLoggingT
                  (do mVaultRes <- fetchVaultAcl
                      case mVaultRes of
                        Nothing ->
                          $logError "Can't retrieve Vault ACL"
                        Just acl ->
                          do mConsulRes <- fetchConsulKV acl
                             case mConsulRes of
                               Nothing ->
                                 $logError "Can't retrieve Git SSH key from Consul"
                               Just (ssh:_) ->
                                 gitAddCommitPullPush ssh
                                                      (appExtra (settings foundation)))
                  logFunc))
     return (logWare $ defaultMiddlewaresNoLogging app,logFunc)

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager
    s <- staticSite

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher
    cache <- newMVar (extraCacheDir (appExtra conf))
    createDirectoryIfMissing True (extraCacheDir (appExtra conf))

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App conf s manager logger cache

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader (fmap fst . makeApplication)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }

-- | Performs the github synchronization with the sig-archive repository
gitAddCommitPullPush :: forall (m :: * -> *) b.
                        (MonadCatch m, MonadLogger m, MonadIO m, Functor m) =>
                        ConsulKVResponse -> Extra -> m b
gitAddCommitPullPush ckr@ConsulKVResponse{..} extra@Extra{..} =
  (do homeEnv <- liftIO (E.getEnv "HOME")
      homePath <- parseAbsDir homeEnv
      let sshKeyPath =
            homePath </>
            $(mkRelDir ".ssh") </>
            $(mkRelFile "id_rsa_sig_archive")
          gitSshCmd =
            Just [("GIT_SSH_COMMAND"
                  ,"ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no")]
      -- wait
      liftIO (threadDelay (1000 * 1000 * 60 * extraPullMinutes))
      -- git config
      liftIO
        (do void (readProcess "git"
                              ["config","--global","user.name","Sig Service"]
                              "")
            void (readProcess
                    "git"
                    ["config","--global","user.email","dev@fpcomplete.com"]
                    "")
            void (readProcess "git"
                              ["config","--global","push.default","current"]
                              ""))
      -- drop ssh key
      liftIO
        (do createDirectoryIfMissing True
                                     (toFilePath . parent $ sshKeyPath)
            setFileMode (toFilePath . parent $ sshKeyPath)
                        ownerModes
            writeFile (toFilePath sshKeyPath)
                      (T.unpack ckrValue)
            setFileMode (toFilePath sshKeyPath)
                        (unionFileModes ownerReadMode ownerWriteMode))
      -- git clone
      liftIO
        (do exists <- doesDirectoryExist extraRepoPath
            unless exists
                   (void (do (_,_,_,clone) <-
                               createProcess
                                 (proc "git"
                                       ["-q"
                                       ,"-b"
                                       ,extraRepoBranch
                                       ,extraRepoUrl
                                       ,extraRepoPath]) {env = gitSshCmd}
                             void (waitForProcess clone)))) `catches`
        [Handler (\ex ->
                    $logInfo ("Not able to clone the git repo. " <>
                              (fromString (show (ex :: IOError)))))]
      -- git add & commit
      liftIO
        (do void (readProcess "git"
                              ["-C",extraRepoPath,"add","-A"]
                              "")
            void (readProcess
                    "git"
                    ["-C"
                    ,extraRepoPath
                    ,"commit"
                    ,"-m"
                    ,"sig-service automated commit"]
                    "")) `catches`
        [Handler (\ex ->
                    $logInfo ("Not able to commit anything at this time. " <>
                              (fromString (show (ex :: IOError)))))]
      -- git pull/push
      liftIO
        (do (_,_,_,pull) <-
              createProcess
                (proc "git" ["-C",extraRepoPath,"pull","--rebase"]) {env = gitSshCmd}
            void (waitForProcess pull)
            (_,_,_,push) <-
              createProcess (proc "git" ["-C",extraRepoPath,"push"]) {env = gitSshCmd}
            void (waitForProcess push)) `catches`
        [Handler (\ex ->
                    $logError (fromString (show (ex :: IOError))))]
      -- romove ssh key from disk
      liftIO (removeFile (toFilePath sshKeyPath))
      gitAddCommitPullPush ckr extra)

-- | Fetches the Vault credentials lease for the Consul SSH private
-- key secret.
fetchVaultAcl :: forall (m :: * -> *).
                 (MonadBaseControl IO m,MonadIO m,MonadThrow m)
              => m (Maybe VaultLeaseResponse)
fetchVaultAcl =
  do vaultAddr <- liftIO (E.getEnv "VAULT_ADDR")
     vaultToken <-
       liftIO (E.getEnv "VAULT_TOKEN")
     vaultReq <-
       HC.parseUrl (vaultAddr ++ "/v1/consul/creds/sig_service")
     let vaultReq' =
           vaultReq {requestHeaders =
                       [("X-Vault-Token",BC.pack vaultToken)]}
     vaultRes <-
       HC.withManager (HC.httpLbs vaultReq')
     return (A.decode (HC.responseBody vaultRes))

-- | Fetches the SSH private key from Consul using the given Vault
-- credentials lease.
fetchConsulKV :: forall (m :: * -> *).
                 (MonadBaseControl IO m,MonadIO m,MonadThrow m)
              => VaultLeaseResponse -> m (Maybe [ConsulKVResponse])
fetchConsulKV vlr =
  do consulAddr <-
       liftIO (E.getEnv "CONSUL_HTTP_ADDR")
     consulReq <-
       HC.parseUrl (consulAddr ++ "/v1/kv/sig_service/ssh/private")
     let consulReq' =
           HC.setQueryString
             [(T.encodeUtf8 "token"
              ,Just (T.encodeUtf8 (vlrDataToken (vlrData vlr))))]
             consulReq
     consulRes <-
       HC.withManager (HC.httpLbs consulReq')
     return (A.decode (HC.responseBody consulRes))

data VaultLeaseResponse =
  VaultLeaseResponse {vlrId :: !Text
                     ,vlrDuration :: Int
                     ,vlrRenewable :: Bool
                     ,vlrData :: !VaultLeaseResponseData}
  deriving (Show)

data VaultLeaseResponseData =
  VaultLeaseResponseData {vlrDataToken :: !Text}
  deriving (Show)

instance FromJSON VaultLeaseResponse where
  parseJSON (Object v) =
    VaultLeaseResponse <$>
    (v .: "lease_id") <*>
    (v .: "lease_duration") <*>
    (v .: "renewable") <*>
    (v .: "data")
  parseJSON _ = mzero

instance FromJSON VaultLeaseResponseData where
  parseJSON (Object v) =
    VaultLeaseResponseData <$>
    (v .: "token")
  parseJSON _ = mzero

data ConsulKVResponse =
  ConsulKVResponse {ckrKey :: Text
                   ,ckrCreateIndex :: Int
                   ,ckrModifyIndex :: Int
                   ,ckrLockIndex :: Int
                   ,ckrFlags :: Int
                   ,ckrValue :: Text}
  deriving (Show)

instance FromJSON ConsulKVResponse where
  parseJSON (Object v) =
    ConsulKVResponse <$>
    (v .: "Key") <*>
    (v .: "CreateIndex") <*>
    (v .: "ModifyIndex") <*>
    (v .: "LockIndex") <*>
    (v .: "Flags") <*>
    (v .: "Value")
  parseJSON _ = mzero
