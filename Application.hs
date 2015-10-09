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

import           Control.Monad.Error
import qualified Data.Aeson as A
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Void
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
       (forkIO (runLoggingT (vacuous (gitAddCommitPullPush (appExtra (settings foundation))))
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
gitAddCommitPullPush :: forall (m :: * -> *).
                        (MonadBaseControl IO m,MonadCatch m,MonadMask m,MonadLogger m,MonadIO m,Functor m)
                     => Extra -> m Void
gitAddCommitPullPush extra@Extra{..} =
  do homeEnv <- liftIO (E.getEnv "HOME")
     homePath <- parseAbsDir homeEnv
     let sshKeyPath =
           homePath </>
           $(mkRelDir ".ssh") </>
           $(mkRelFile "id_rsa_sig_archive")
         name = "Sig Service"
         email = "dev@fpcomplete.com"
         gitEnv =
           Just [("HOME",homeEnv)
                ,("GIT_SSH_COMMAND"
                 ,"ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -i " ++
                  toFilePath sshKeyPath)
                ,("GIT_CONFIG_NOSYSTEM","1")
                ,("GIT_AUTHOR_NAME",name)
                ,("GIT_AUTHOR_EMAIL",email)
                ,("GIT_COMMITTER_NAME",name)
                ,("GIT_COMMITTER_EMAIL",email)]
     mVaultRes <- fetchVaultAcl
     case mVaultRes of
       Nothing ->
         $logError "Can't retrieve vault acl for sig-archive git ssh key"
       Just acl ->
         do mConsulRes <- fetchConsulKV acl
            case mConsulRes of
              Nothing ->
                $logError "Can't retrieve sig-service git ssh key from consul"
              Just (ssh:_) ->
                case (B64.decode . T.encodeUtf8) (ckrValue ssh) of
                  Left err ->
                    $logError (T.pack ("Can't extract sig-service git ssh key received from consul: " ++
                                       err))
                  Right ssh' ->
                    bracket (writeSshKey sshKeyPath
                                         (BC.unpack ssh'))
                            (const (liftIO (removeFile (toFilePath sshKeyPath))))
                            (const (do gitClone extra gitEnv
                                       gitAddAndCommit extra gitEnv
                                       gitPullAndPush extra gitEnv))
     liftIO (threadDelay (1000 * 1000 * 60 * extraPullMinutes))
     gitAddCommitPullPush extra

writeSshKey :: forall (m :: * -> *).
               (MonadCatch m,MonadLogger m,MonadIO m)
            => Path Abs File -> String -> m ()
writeSshKey path key =
  do let dir = toFilePath (parent path)
     liftIO (createDirectoryIfMissing True dir)
     liftIO (setFileMode dir ownerModes)
     liftIO (writeFile (toFilePath path) key)
     liftIO (setFileMode (toFilePath path)
                         (unionFileModes ownerReadMode ownerWriteMode))

gitClone :: forall (m :: * -> *).
            (MonadCatch m,MonadLogger m,MonadIO m,Functor m)
         => Extra -> Maybe [(String,String)] -> m ()
gitClone Extra{..} gitEnv =
  do exists <-
       liftIO (doesDirectoryExist extraRepoPath)
     unless exists
            (do (_,_,_,clone) <-
                  liftIO (createProcess
                            (proc "git"
                                  ["-q"
                                  ,"-b"
                                  ,extraRepoBranch
                                  ,extraRepoUrl
                                  ,extraRepoPath]) {env = gitEnv})
                void (liftIO (waitForProcess clone))) `catches`
       [Handler (\ex ->
                   $logInfo ("Not able to clone the git repo. " <>
                             (fromString (show (ex :: IOError)))))]

gitAddAndCommit :: forall (m :: * -> *).
                   (MonadCatch m,MonadLogger m,MonadIO m,Functor m)
                => Extra -> Maybe [(String, String)] -> m ()
gitAddAndCommit Extra{..} gitEnv =
  (do (_,_,_,add) <-
        liftIO (createProcess (proc "git" ["-C",extraRepoPath,"add","-A"]) {env = gitEnv})
      void (liftIO (waitForProcess add))
      (_,_,_,commit) <-
        liftIO (createProcess
                  (proc "git"
                        ["-C"
                        ,extraRepoPath
                        ,"commit"
                        ,"-m"
                        ,"sig-service automated commit"]) {env = gitEnv})
      void (liftIO (waitForProcess commit))) `catches`
  [Handler (\ex ->
              $logInfo ("Not able to commit anything at this time. " <>
                        (fromString (show (ex :: IOError)))))]

gitPullAndPush :: forall (m :: * -> *).
                  (MonadCatch m,MonadLogger m,MonadIO m,Functor m)
               => Extra -> Maybe [(String, String)] -> m ()
gitPullAndPush Extra{..} gitEnv =
  (do (_,_,_,pull) <-
        liftIO (createProcess (proc "git" ["-C",extraRepoPath,"pull","--rebase"]) {env = gitEnv})
      void (liftIO (waitForProcess pull))
      (_,_,_,push) <-
        liftIO (createProcess (proc "git" ["-C",extraRepoPath,"push"]) {env = gitEnv})
      void (liftIO (waitForProcess push))) `catches`
  [Handler (\ex ->
              $logError (fromString (show (ex :: IOError))))]

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
