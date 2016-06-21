{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Bugsnag.Types
import System.Environment
import Network.HTTP.Dispatch.Core
import Network.HTTP.Dispatch.Types
import Data.ByteString.Lazy as BL
import Data.Text
import qualified Data.Text as T
import Data.Aeson

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

main :: IO ()
main = do
  key <- appKey
  runRequest (myHttpReq key)
  return ()
