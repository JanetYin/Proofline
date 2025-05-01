{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MainTest where

import Control.Exception
import System.Environment
import System.Exit
import Data.Aeson (ToJSON(..), object, (.=))
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy as BSL

import Common
import qualified Context
import Errors
import Evaluation
import Metacontext hiding (expectedType)
import Parser
import qualified Pretty
import Elaboration
import qualified Display
import Syntax
import Value
import qualified Presyntax as P

--------------------------------------------------------------------------------

helpMsg :: String 
helpMsg = unlines [
  "usage: proofline [--help|elaborate|normalize|type|json]",
  "  --help : display this message",
  "  elaborate     : read & elaborate expression from stdin",
  "  normalize     : read & typecheck expression from stdin, print its normal form and type",
  "  type          : read & typecheck expression from stdin, print its type",
  "  json          : read & elaborate expression from stdin, output JSON with holes"]

-- Creating a newtype wrapper for MetaEntry to avoid orphan instance
newtype MetaEntryJSON = MetaEntryJSON MetaEntry

-- Define JSON instance for the wrapped type
instance ToJSON MetaEntryJSON where
  toJSON (MetaEntryJSON (Solved val)) = object [
    "solved" .= True,
    "value" .= showTm0 (quote 0 val)
    ]
  toJSON (MetaEntryJSON (Unsolved ctx expType)) = 
    let names = map fst ctx
        level = Lvl (length ctx)
        quotedType = quote level expType
        typeStr = Pretty.prettyTm 0 names quotedType []
    in 
      object [
    "solved" .= False,
    "value" .= ("" :: String),
    "expectedType" .= typeStr,  
    "context" .= map showContextEntry ctx]

-- Helper function to display a context entry
showContextEntry :: (Name, VTy) -> String
showContextEntry (name, val) = name ++ " : " ++ showVal0 val

-- Use the showTm0 function from Pretty
showTm0 :: Tm -> String
showTm0 = Pretty.showTm0

formatJSON :: JSON.Value -> BSL.ByteString
formatJSON = JSON.encode

-- Get all metavariables as JSON
getHolesAsJSON :: IO JSON.Value
getHolesAsJSON = do
  metaEntries <- getMetas
  
  putStrLn "Metavariables:"
  mapM_ Display.displayMeta metaEntries
  
  return $ JSON.toJSON $ map convertMetaToJSON metaEntries
  where
    -- Helper function to convert a meta entry to JSON
    convertMetaToJSON :: (Int, MetaEntry) -> JSON.Value
    convertMetaToJSON (k, entry) = case entry of
      Solved val -> 
        object [
          "id" .= k,
          "isSolved" .= True,
          "value" .= showTm0 (quote 0 val)
        ]
      
      Unsolved ctx expType -> do
        let names = map fst ctx
            level = Lvl (length ctx)
            quotedType = quote level expType
            typeStr = Pretty.prettyTm 0 names quotedType []
            
        let contextEntries = processContext ctx
            
        object [
          "id" .= k,
          "isSolved" .= False,
          "expectedType" .= typeStr,
          "context" .= contextEntries]
    
    -- Process context with proper name resolution (similar to displayWithNames) how is this function working? why needed?

    processContext :: [(Name, VTy)] -> [String]
    processContext ctx = go (reverse ctx) []
      where
        go [] _ = []
        go ((name, val):rest) names = 
          let depth = Lvl (length names)
              quotedVal = quote depth val
              prettyVal = Pretty.prettyTm 0 names quotedVal []
              entry = name ++ " : " ++ prettyVal
          in entry : go rest (name:names)

-- Run elaboration and return the term and its type
elaborate :: P.Raw -> String -> IO (Tm, Value)
elaborate t file = do
  Metacontext.reset  -- Reset metavariable state, why?
  infer (Context.emptyContext (initialPos file)) t
    `catch` \e -> Display.displayError file e >> exitSuccess

mainWith :: IO [String] -> IO (P.Raw, String) -> IO ()
mainWith getOpt getRaw = do
  getOpt >>= \case
    ["--help"] -> putStrLn helpMsg
    ["normalize"] -> do
      (t, file) <- getRaw
      (term, ty) <- elaborate t file
      putStrLn $ "Value : Type"
      putStrLn $ showTm0 (nf [] term) ++ " : " ++ showTm0 (quote 0 ty)
    ["type"] -> do
      (t, file) <- getRaw
      (_, ty) <- elaborate t file 
      putStrLn $ showTm0 $ quote 0 ty
    ["elaborate"] -> do
      (t, file) <- getRaw
      (term, _) <- elaborate t file
      Display.displayMetas
      putStrLn $ showTm0 term
    ["json"] -> do
      (t, file) <- getRaw
      result <- try $ do
        (term, ty) <- elaborate t file
        putStrLn $ showTm0 term
        holes <- getHolesAsJSON
        let jsonResult = object [
              "success" .= True,
              "term" .= showTm0 term,
              "type" .= showTm0 (quote 0 ty),
              "state" .= object [
                "script" .= show t, 
                "holes" .= holes    
              ]]
        return jsonResult
      case result of
        Left (e :: Error) -> do
          let errorMsg = "Error: " ++ show e
              errorJson = object [
                "success" .= False,
                "error" .= errorMsg ]
          BS.putStrLn $ formatJSON errorJson
        Right jsonResult ->
          BS.putStrLn $ formatJSON jsonResult
    _ -> putStrLn helpMsg

main :: IO ()
main = mainWith getArgs parseStdin

-- | Run main with inputs as function arguments.
main' :: String -> String -> IO ()
main' mode src = mainWith (pure [mode]) ((,src) <$> parseString src)

test1 :: IO ()
test1 = main' "elaborate" "λ x . x"

test2 :: IO ()
test2 = main' "elaborate" $ unlines [
  "let id : U → U = λ x. x;",
  "id U"
  ]

test3 :: IO ()
test3 = main' "elaborate" $ unlines [
  "let Pair : U → U → U = λ A B. (x : A) × B;", 
  "let pair : (A : U) → (B : U) → A → B → Pair A B = λ A B x y. (x, y);",
  "pair U U U U"
  ]