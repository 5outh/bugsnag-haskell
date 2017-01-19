{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception
import Network.Bugsnag.TH
import Network.Bugsnag.Types
import System.Environment
import Network.HTTP.Dispatch.Core
import Network.HTTP.Dispatch.Types
import Data.ByteString.Lazy as BL
import Data.Text
import qualified Data.Text as T
import Data.Aeson
import Language.Haskell.TH.Syntax

appKey :: IO Text
appKey = T.pack <$> getEnv "BUGSNAG_APP_KEY"

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
testThread :: BugsnagThread
testThread = BugsnagThread
  "2"
  "Main"
  []

testStackFrame :: BugsnagStackFrame
testStackFrame = BugsnagStackFrame "Main.hs" "myReq" 88 (Just 0) True

testException :: BugsnagException
testException = BugsnagException "errorClass" "There was an error!" [testStackFrame]

testUser :: BugsnagUser
testUser = BugsnagUser (Just "10") (Just "Ben") (Just "ben@ben.com")

testApp :: BugsnagApp
testApp = BugsnagApp (Just "1.0") (Just "development")

testDevice :: BugsnagDevice
testDevice = BugsnagDevice (Just "1.0") (Just "web.io.com")

testRequest :: Text -> BugsnagRequest
testRequest key = BugsnagRequest key [testEvent]

myHttpReq :: Text -> HTTPRequest
myHttpReq key = HTTPRequest
  POST
  "https://notify.bugsnag.com" 
  [header "Content-Type" "application/json"]
  (Just (BL.toStrict (encode $ testRequest key)))

data MyException = MyException
  { exRow :: !Int
  , exColumn :: !Int
  , exName :: !Text
  , exMessage :: !Text
  , exFile :: !Text
  }

instance BugsnagError MyException where
  errorRow = exRow
  errorColumn = Just . exColumn
  errorName = exName
  errorMessage = Just . exMessage
  errorFile = Just . exFile

deriving instance Show MyException

instance Exception MyException

main :: IO ()
main = do
  -- ??? How to get location to show up in proper position
  undefined `catch` \(e :: SomeException) -> do
    print $(qLocation >>= liftLoc)
  key <- appKey
  runRequest (myHttpReq key)
  return ()

