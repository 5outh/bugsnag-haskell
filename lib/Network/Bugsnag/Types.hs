{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Network.Bugsnag.Types where

import Data.Text
import GHC.Generics
import Data.Aeson

text :: Text -> Text
text t = t

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
    , "threads" .= toJSON threads
    , "context" .= toJSON context
    , "groupingHash" .= toJSON groupingHash 
    , "severity" .= toJSON severity 
    , "user" .= toJSON user 
    , "app" .= toJSON app 
    , "device" .= toJSON device 
    , "metaData" .= toJSON metaData 
    ]

data BugsnagThread = BugsnagThread
  { threadId :: Text
  , threadName :: Text
  , threadStackTrace :: [BugsnagStackFrame]
  } deriving (Show, Eq, Generic)

instance ToJSON BugsnagThread where
  toJSON BugsnagThread{..} = object
    [ "id" .= threadId
    , "name" .= threadName
    , "stacktrace" .= toJSON threadStackTrace
    ]

data BugsnagStackFrame = BugsnagStackFrame
  { file :: Text
  , method :: Text
  , lineNumber :: Int
  , columnNumber :: Maybe Int
  , inProject :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON BugsnagStackFrame

data BugsnagException = BugsnagException
  { errorClass :: Text
  , message :: Text
  , stacktrace :: [BugsnagStackFrame]
  } deriving (Show, Eq, Generic)

instance ToJSON BugsnagException

data BugsnagUser = BugsnagUser
  { id :: Maybe Text
  , name :: Maybe Text
  , email :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON BugsnagUser

data BugsnagApp  = BugsnagApp
  { version :: Maybe Text
  , releaseStage :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON BugsnagApp

data BugsnagDevice = BugsnagDevice
  { osVersion :: Maybe Text
  , hostname :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON BugsnagDevice

data BugsnagRequest = BugsnagRequest
  { apiKey :: Text 
  , events :: [BugsnagEvent]
  } deriving (Show, Eq, Generic)

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

class BugsnagError e where
  errorRow :: e -> Int
  errorColumn :: e -> Maybe Int
  errorName :: e -> Text
  errorMessage ::  e -> Maybe Text
  errorFile :: e -> Maybe Text

