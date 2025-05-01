{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}  -- Added this extension for record wildcards

module API 
  ( runServer
  , ProoflineAPI
  ) where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, FromJSON, toJSON, object, (.=))  -- Import object and .= explicitly
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import Servant
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))

-- Import proof assistant core modules
import qualified Common
import qualified Context
import qualified Elaboration
import qualified Errors
import qualified Evaluation
import qualified Metacontext
import qualified Parser
import qualified Pretty
import qualified Syntax
import qualified Value

-- ===============================
-- Helper Functions
-- ===============================

-- Helper functions for displaying terms and values
showTm0 :: Syntax.Tm -> String
showTm0 = Pretty.showTm0

showVal0 :: Value.Value -> String
showVal0 v = Pretty.prettyTm 0 [] (Evaluation.quote 0 v) []

-- ===============================
-- Data Types
-- ===============================

data Hole = Hole
  { holeId :: Int
  , isSolved :: Bool
  , value :: Text            -- Value for solved holes, underscore for unsolved
  , expectedType :: Text     -- Type of the hole, empty for solved holes
  , context :: [Text]        -- Context entries, empty for solved holes
  } deriving (Eq, Show, Generic)

data ProofState = ProofState
  { script :: Text
  , fileName :: Text
  , holes :: [Hole]
  , errorMessage :: Maybe Text
  } deriving (Eq, Show, Generic)

-- Modified ApiResponse to include term and type fields
data ApiResponse = ApiResponse
  { success :: Bool
  , message :: Maybe Text
  , term :: Maybe Text       -- Added term field
  , type_ :: Maybe Text      -- Added type field (using type_ to avoid Haskell keyword)
  , state :: Maybe ProofState
  } deriving (Eq, Show, Generic)

-- Request data types
data CheckProofRequest = CheckProofRequest
  { checkScript :: Text
  } deriving (Eq, Show, Generic)

data SaveFileRequest = SaveFileRequest
  { saveFileName :: Text
  , saveContent :: Text
  } deriving (Eq, Show, Generic)

-- JSON instances
instance ToJSON Hole
instance FromJSON Hole
instance ToJSON ProofState
instance FromJSON ProofState
instance ToJSON ApiResponse where
  toJSON ApiResponse{..} = object [
      "success" .= success
    , "message" .= message
    , "term" .= term        -- Include term in JSON
    , "type" .= type_       -- Include type in JSON
    , "state" .= state
    ]
instance FromJSON ApiResponse
instance ToJSON CheckProofRequest
instance FromJSON CheckProofRequest
instance ToJSON SaveFileRequest
instance FromJSON SaveFileRequest

-- ===============================
-- API Definition
-- ===============================

type ProoflineAPI =
  "api" :> "check" :> ReqBody '[JSON] CheckProofRequest :> Post '[JSON] ApiResponse
  :<|> "api" :> "save" :> ReqBody '[JSON] SaveFileRequest :> Post '[JSON] ApiResponse
  :<|> "api" :> "load" :> QueryParam "fileName" Text :> Get '[JSON] ApiResponse
  -- Test endpoint for simple API connection verification
  :<|> "api" :> "test" :> Get '[JSON] ApiResponse

-- ===============================
-- Core Logic Helpers
-- ===============================

-- Get holes from metacontext
getHoles :: IO [Hole]
getHoles = do
  metaEntries <- Metacontext.getMetas
  return $ map (\(metaId, status) -> 
    case status of
      Metacontext.Solved val -> 
        Hole 
          metaId 
          True 
          (T.pack $ showVal0 val) 
          "" 
          []
          
      Metacontext.Unsolved ctx expectedType -> 
        -- Handle context and type with proper names
        let names = map fst ctx
            level = Common.Lvl (length ctx)
            quotedType = Evaluation.quote level expectedType
            typeStr = Pretty.prettyTm 0 names quotedType []
            
            -- Create context entries with correct naming
            contextEntries = processContext (reverse ctx)
        in 
          Hole 
            metaId 
            False 
            "_" 
            (T.pack typeStr) 
            (map T.pack contextEntries)
    ) metaEntries
  where
    -- Helper function to process context with proper name resolution
    processContext :: [(Common.Name, Value.VTy)] -> [String]
    processContext ctx = go ctx []
      where
        go [] _ = []
        go ((name, val):rest) names = 
          let depth = Common.Lvl (length names)
              quotedVal = Evaluation.quote depth val
              prettyVal = Pretty.prettyTm 0 names quotedVal []
              entry = name ++ " : " ++ prettyVal
          in entry : go rest (name:names)

-- Parse and elaborate code, return current state with term and type
parseAndElaborate :: String -> IO (Either String (ProofState, Text, Text))
parseAndElaborate src = do
  -- Reset metavariable state
  Metacontext.reset
  
  -- Try parsing
  parseResult <- Parser.parseStringWithErrors src
  
  case parseResult of
    Left errorMsg -> do
      -- Now we're capturing the actual error message from the parser
      return $ Left errorMsg
    
    Right rawExpr -> do
      -- Parsing successful, try elaboration
      elaborationResult <- try $ do
        (elaboratedTerm, typeValue) <- Elaboration.infer (Context.emptyContext (Common.initialPos "input")) rawExpr
        metaHoles <- getHoles
        
        -- Convert term and type to Text
        let termText = T.pack $ showTm0 elaboratedTerm
            typeText = T.pack $ showTm0 (Evaluation.quote 0 typeValue)
        
        return (ProofState
          { script = T.pack src
          , fileName = "current.proof"
          , holes = metaHoles
          , errorMessage = Nothing
          }, 
          termText,
          typeText)
      
      case elaborationResult of
        Left (e :: Errors.Error) -> do
          -- Format error message
          let errorMsg = case e of
                Errors.Error cxt (Errors.UnboundVar name) -> 
                  "Unbound variable: " ++ name ++ " (position: " ++ showPos (Context.pos cxt) ++ ")"
                Errors.Error cxt (Errors.CantUnify t1 t2) -> 
                  "Type mismatch (position: " ++ showPos (Context.pos cxt) ++ "):\n  " ++ showTm0 t1 ++ "\nand\n  " ++ showTm0 t2
                Errors.Error cxt (Errors.NameNotInScope name) ->
                  "Name not in scope: " ++ name ++ " (position: " ++ showPos (Context.pos cxt) ++ ")"
                -- Removed redundant pattern case
                
          return $ Left errorMsg
          
        Right (proofState, termText, typeText) -> 
          return $ Right (proofState, termText, typeText)
  where
    showPos (Common.SourcePos _ line col) =
      "line " ++ show (Common.unPos line) ++ ", column " ++ show (Common.unPos col)

-- ===============================
-- Server Implementation
-- ===============================

server :: Server ProoflineAPI
server = checkProof
    :<|> saveFile
    :<|> loadFile
    :<|> testEndpoint
  where
    -- File directory configuration
    proofDirectory = "proofs"  -- Proof files save directory
    
    -- Check proof
    checkProof :: CheckProofRequest -> Handler ApiResponse
    checkProof req = do
      result <- liftIO $ parseAndElaborate (T.unpack $ checkScript req)
      case result of
        Left errorMsg -> 
          return $ ApiResponse False (Just $ T.pack errorMsg) Nothing Nothing Nothing
        Right (proofState, termText, typeText) -> 
          return $ ApiResponse True Nothing (Just termText) (Just typeText) (Just proofState)
    
    -- Save file
    saveFile :: SaveFileRequest -> Handler ApiResponse
    saveFile req = do
      -- Ensure proof directory exists
      liftIO $ createDirectoryIfMissing True proofDirectory
      
      let filePath = proofDirectory </> T.unpack (saveFileName req)
      liftIO $ TIO.writeFile filePath (saveContent req)
      return $ ApiResponse True (Just "File saved successfully") Nothing Nothing Nothing
    
    -- Load file with term and type
    loadFile :: Maybe Text -> Handler ApiResponse
    loadFile Nothing = 
      return $ ApiResponse False (Just "File name is required") Nothing Nothing Nothing
    loadFile (Just fileNameText) = do
      let filePath = proofDirectory </> T.unpack fileNameText
      fileExists <- liftIO $ doesFileExist filePath
      if fileExists
        then do
          content <- liftIO $ TIO.readFile filePath
          result <- liftIO $ parseAndElaborate (T.unpack content)
          case result of
            Left errorMsg -> 
              return $ ApiResponse False (Just $ T.pack errorMsg) Nothing Nothing Nothing
            Right (proofState, termText, typeText) -> 
              return $ ApiResponse 
                        True 
                        Nothing 
                        (Just termText) 
                        (Just typeText) 
                        (Just $ proofState { fileName = fileNameText })
        else
          return $ ApiResponse False (Just $ "File not found: " <> fileNameText) Nothing Nothing Nothing
    
    -- Test endpoint
    testEndpoint :: Handler ApiResponse
    testEndpoint = do
      liftIO $ putStrLn "Test endpoint called"
      return $ ApiResponse True (Just "API connection test successful") Nothing Nothing Nothing

-- Modified CORS policy to allow all origins
corsPolicy :: CorsResourcePolicy
corsPolicy = simpleCorsResourcePolicy
  { corsOrigins = Nothing  -- Allow all origins
  , corsMethods = ["GET", "POST", "PUT", "OPTIONS"]  -- Allowed HTTP methods
  , corsRequestHeaders = ["Content-Type", "Accept"]  -- Allowed request headers
  , corsExposedHeaders = Nothing
  , corsMaxAge = Just 3600  -- Cache preflight request results
  }

-- Modified server startup code
runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "Starting Proofline API server on port " ++ show port
  putStrLn $ "Test endpoint available at: http://localhost:" ++ show port ++ "/api/test"
  createDirectoryIfMissing True "proofs"
  run port $ cors (\_ -> Just corsPolicy) $ serve (Proxy :: Proxy ProoflineAPI) server