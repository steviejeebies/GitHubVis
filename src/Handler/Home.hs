{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-} -- needed for \case

module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import GHC.Generics ( Generic )
import Data.Aeson
import MyGitHubAPI

-- Imports required for Servant calls
import qualified Servant.Client as SClient
import Servant.API ( BasicAuthData(BasicAuthData) )
import System.Environment ( getEnv )
import Data.Text as DT
import Data.List as DL ( map )
import Data.Either
import qualified Data.ByteString.UTF8 as U8 (fromString)
import Configuration.Dotenv as Dotenv (loadFile, defaultConfig)
import Control.Monad

import System.Random (randomRIO)
-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "SWENG PROJECT"
        $(widgetFile "homepage")
        $(widgetFile "charts/barchart")
        $(widgetFile "charts/histogram")
        $(widgetFile "charts/piechart")
        $(widgetFile "userInputFormSubmission")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "SWENG PROJECT"
        $(widgetFile "homepage")

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

-- data User = User { name :: String
--                  , commits :: Int
--                  } deriving (Generic, ToJSON)

-- getUserDatamR :: Handler Value
-- getUserDatamR = do
--   lst <- liftIO $ randomList ["Bill", "Ben","John", "Jane", "Tim", "Sam"]
--   return $ toJSON lst

-- randomList :: [String] -> IO [User]
-- randomList [] = return []
-- randomList (x:xs) = do
--   r  <- randomRIO (10,40)
--   rs <- randomList xs
--   return (User x r:rs)

headerUserAgent :: Maybe GHHeaderUserAgent
headerUserAgent = Just "user-agent"

headerAccept :: Maybe GHHeaderAccept
headerAccept = Just "application/vnd.github.v3+json"  -- explicitly stating that we are using API v3, as specified in the GitHub documentation

getAuthToken :: IO BasicAuthData
getAuthToken = do
    loadFile defaultConfig
    tokenUser <- getEnv "GITHUBUSER"
    tokenString <- getEnv "GITHUBTOKEN"
    let authToken = BasicAuthData (U8.fromString tokenUser) (U8.fromString tokenString)  -- Creating authentication token to pass
    return authToken

dud :: GHUserData
dud = GHUserData Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 

getUserDataR :: Text -> Handler Value
getUserDataR ghUserName = do
    authToken <- liftIO getAuthToken             -- separated into its own function 
    responce <- liftIO $ SClient.runClientM (MyGitHubAPI.getGHUserName headerUserAgent headerAccept authToken ghUserName) =<< MyGitHubAPI.env
    return $ toJSON $ fromRight dud responce
    

-- newtype Natural = Natural Int
--     deriving (Eq, Show, Read)

-- instance PathPiece Natural where
--     toPathPiece (Natural i) = DT.pack $ show i
--     fromPathPiece s =
--         case reads $ DT.unpack s of
--             (i, ""):_
--                 | i < 1 -> Nothing
--                 | otherwise -> Just $ Natural i
--             [] -> Nothing

-- getGHUserDataR :: Handler Value



