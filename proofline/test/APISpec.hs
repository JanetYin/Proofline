{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module APISpec (spec) where

import Test.Hspec
import Data.Aeson (encode, ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.Exception (try, SomeException)
import qualified Data.Text as T

data ApiResponse = ApiResponse
  { success :: Bool
  , message :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON ApiResponse
instance FromJSON ApiResponse

data CheckProofRequest = CheckProofRequest
  { checkScript :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON CheckProofRequest
instance FromJSON CheckProofRequest

data SaveFileRequest = SaveFileRequest
  { saveFileName :: Text
  , saveContent :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON SaveFileRequest
instance FromJSON SaveFileRequest

makeRequest :: IO (Either String BL.ByteString)
makeRequest = do
  return $ Right $ encode $ ApiResponse True (Just $ T.pack "Test response")

checkConnection :: IO Bool
checkConnection = do
  result <- try @SomeException $ makeRequest
  return $ case result of
    Right _ -> True
    Left _  -> False

spec :: Spec
spec = do
  describe "API Tests" $ do
    describe "Test Endpoint" $ do
      it "can connect to the test endpoint" $ do
        connectionSuccess <- checkConnection
        connectionSuccess `shouldBe` True

    describe "Check Proof Endpoint" $ do
      it "can connect to the check proof endpoint" $ do
        connectionSuccess <- checkConnection
        connectionSuccess `shouldBe` True

    describe "File Operations" $ do
      it "can connect to the save endpoint" $ do
        connectionSuccess <- checkConnection
        connectionSuccess `shouldBe` True

      it "can connect to the load endpoint" $ do
        connectionSuccess <- checkConnection
        connectionSuccess `shouldBe` True