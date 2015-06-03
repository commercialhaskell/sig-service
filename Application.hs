{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
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
import           Data.String (fromString)
import           Import
import           Network.HTTP.Client.Conduit (newManager)
import           Network.Wai.Logger (clockDateCacher)
import           Network.Wai.Middleware.RequestLogger ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination)
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import           System.Directory
import           System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import           System.Process (readProcess)
import           Yesod.Core.Types (loggerSet, Logger (Logger))
import           Yesod.Default.Config
import           Yesod.Default.Handlers
import           Yesod.Default.Main

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import           Handler.Home
import           Handler.UploadSig
import           Handler.DownloadArchive

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
     -- Clone sig-archive
     let Extra{..} =
           (appExtra (settings foundation))
     -- Git Add/Commit/Pull/Push every X minutes
     void
       (forkIO (runLoggingT
                  (do gitAddCommitPullPush extraRepoUrl
                                           extraRepoBranch
                                           extraRepoPath
                                           extraPullMinutes)
                  logFunc))
     return (logWare $ defaultMiddlewaresNoLogging app,logFunc)
  where gitAddCommitPullPush url branch dir minutes =
          (do liftIO (threadDelay (1000 * 1000 * 60 * minutes))
              exists <-
                liftIO (doesDirectoryExist dir)
              unless exists
                     (void (liftIO (readProcess
                                      "git"
                                      ["clone","-q","-b",branch,url,dir]
                                      "")))
              void (liftIO (readProcess
                              "git"
                              ["-C"
                              ,dir
                              ,"add"
                              ,"-A"]
                              "" >>
                           readProcess
                              "git"
                              ["-C"
                              ,dir
                              ,"commit"
                              ,"-m"
                              ,"sig-service automated commit"]
                              "")) `catches`
                [Handler (\ex ->
                            $logInfo ("Not able to commit anything at this time. " <>
                                      (fromString (show (ex :: IOError)))))]
              void (liftIO (readProcess "git"
                                        ["-C",fromString dir,"pull","--rebase"]
                                        "" >>
                            readProcess "git"
                                        ["-C",fromString dir,"push"]
                                        "")) `catches`
                [Handler (\ex ->
                            $logError (fromString (show (ex :: IOError))))]
              gitAddCommitPullPush url branch dir minutes)

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
