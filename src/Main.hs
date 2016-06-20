{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Network.HTTP.Dispatch.Core
import Network.HTTP.Dispatch.Types
import Data.ByteString.Lazy as BL
import Data.Text
import qualified Data.Text as T
import GHC.Generics
import Data.Aeson

appKey :: IO Text
appKey = T.pack <$> getEnv "BUGSNAG_APP_KEY"

data BugsnagEvent = BugsnagEvent
  { exceptions :: [BugsnagException]
  , threads :: Maybe [BugsnagThread]
  , context :: Maybe Text 
  , groupingHash :: Maybe Text
  , severity :: Maybe Text
  , user :: BugsnagUser 
  , app :: BugsnagApp 
  , device :: BugsnagDevice
  , metaData :: Maybe Value 
  } deriving (Show, Eq, Generic)


instance ToJSON BugsnagEvent where
  toJSON BugsnagEvent{..} = object
    [ "payloadVersion" .= text "2"
    , "exceptions" .= toJSON exceptions
    -- , "threads" .= toJSON threads
    -- , "context" .= toJSON context
    -- , "groupingHash" .= toJSON groupingHash 
    -- , "severity" .= toJSON severity 
    -- , "user" .= toJSON user 
    -- , "app" .= toJSON app 
    -- , "device" .= toJSON device 
    -- , "metaData" .= toJSON metaData 
    ]

testEvent :: BugsnagEvent
testEvent = BugsnagEvent
  [testException]
  (Just [testThread])
  (Just "context")
  (Just "groupingHash")
  (Just "warning")
  testUser
  testApp
  testDevice
  Nothing

data BugsnagThread = BugsnagThread
  deriving (Show, Eq, Generic)

testThread :: BugsnagThread
testThread = BugsnagThread

data BugsnagStackFrame = BugsnagStackFrame
  { file :: Text
  , method :: Text
  , lineNumber :: Int
  , columnNumber :: Maybe Int
  , inProject :: Bool
  } deriving (Show, Eq, Generic)

testStackFrame :: BugsnagStackFrame
testStackFrame = BugsnagStackFrame "Main.hs" "myReq" 88 (Just 0) True

instance ToJSON BugsnagStackFrame

data BugsnagException = BugsnagException
  { errorClass :: Text
  , message :: Text
  , stacktrace :: [BugsnagStackFrame]
  } deriving (Show, Eq, Generic)

testException :: BugsnagException
testException = BugsnagException "errorClass" "There was an error!" [testStackFrame]

data BugsnagUser = BugsnagUser
  { id :: Maybe Text
  , name :: Maybe Text
  , email :: Maybe Text
  } deriving (Show, Eq, Generic)

testUser :: BugsnagUser
testUser = BugsnagUser (Just "10") (Just "Ben") (Just "ben@ben.com")

data BugsnagApp  = BugsnagApp
  { version :: Maybe Text
  , releaseStage :: Maybe Text
  } deriving (Show, Eq, Generic)

testApp :: BugsnagApp
testApp = BugsnagApp (Just "1.0") (Just "development")

data BugsnagDevice = BugsnagDevice
  { osVersion :: Maybe Text
  , hostname :: Maybe Text
  } deriving (Show, Eq, Generic)

testDevice :: BugsnagDevice
testDevice = BugsnagDevice (Just "1.0") (Just "web.io.com")

instance ToJSON BugsnagThread
instance ToJSON BugsnagException
instance ToJSON BugsnagUser
instance ToJSON BugsnagApp
instance ToJSON BugsnagDevice

data BugsnagRequest = BugsnagRequest
  { apiKey :: Text 
  , events :: [BugsnagEvent]
  } deriving (Show, Eq, Generic)

testRequest :: Text -> BugsnagRequest
testRequest key = BugsnagRequest key [testEvent]

text :: Text -> Text
text t = t

instance ToJSON BugsnagRequest where
  toJSON BugsnagRequest{..} =
    object
      [ "apiKey" .= apiKey
      , "notifier" .= object
        [ "name" .= text "Haskell Notifier"
        , "version" .= text "1.0.1.1"
        , "url" .= text "https://github.com/5outh/bugsnag-haskell"
        ]
      , "events" .= toJSON events
      ]

-- TODO: Move everything inside events, because that's where it lives for some
-- reason.

myHttpReq :: Text -> HTTPRequest
myHttpReq key = HTTPRequest
  POST
  "https://notify.bugsnag.com" 
  [header "Content-Type" "application/json"]
  (Just (BL.toStrict (encode $ testRequest key)))

main :: IO ()
main = do
  key <- appKey
  runRequest (myHttpReq key)
  return ()
