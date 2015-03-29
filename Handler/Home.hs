{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import qualified Data.Map.Strict as M

import qualified Data.Set as S
import           Data.Text.Encoding (decodeUtf8)
import           Distribution.Text (display)
import           Import
import           Sig.Archive
import           Sig.Types
import           Text.Email.Parser

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    extra <- getExtra
    archive <- liftIO (readArchive (extraRepoPath extra))
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "sig-service"
        $(widgetFile "homepage")
