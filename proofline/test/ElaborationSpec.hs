module ElaborationSpec (spec) where

import Test.Hspec
import Control.Exception (try)

import Common
import Context
import Elaboration
import Errors
import Evaluation
import Metacontext hiding (expectedType)
import Parser
import Syntax
import Value

-- Helper function to elaborate a term from string
elaborateString :: String -> IO (Either Error (Tm, Value))
elaborateString src = do
  -- Reset metacontext state
  Metacontext.reset
  
  -- Parse and elaborate
  result <- try $ do
    raw <- Parser.parseString src
    Elaboration.infer (Context.emptyContext (initialPos "(test)")) raw
  
  return result

-- Helper function to only check if elaboration succeeds
elaborationSucceeds :: String -> IO Bool
elaborationSucceeds src = do
  result <- elaborateString src
  return $ case result of
    Right _ -> True
    Left _ -> False

-- Helper function to check the type of a term
checkHasType :: String -> String -> IO Bool
checkHasType src expectedTypeStr = do
  result <- elaborateString src
  case result of
    Right (_, actualType) -> do
      let typeStr = showTm0 (quote 0 actualType)
      return $ typeStr == expectedTypeStr
    Left _ -> return False

-- Helper to show terms as strings
showTm0 :: Tm -> String
showTm0 tm = case tm of
  U -> "U"
  Pi "_" a b -> showTm0 a ++ " → " ++ showTm0 b
  Pi x a b -> "(" ++ x ++ " : " ++ showTm0 a ++ ") → " ++ showTm0 b
  Lam x t -> "λ" ++ x ++ ". " ++ showTm0 t
  App t u -> "(" ++ showTm0 t ++ " " ++ showTm0 u ++ ")"
  Var (Ix i) -> "var" ++ show i
  Sigma "_" a b -> showTm0 a ++ " × " ++ showTm0 b
  Sigma x a b -> "(" ++ x ++ " : " ++ showTm0 a ++ ") × " ++ showTm0 b
  Pair a b -> "(" ++ showTm0 a ++ ", " ++ showTm0 b ++ ")"
  Proj1 t -> showTm0 t ++ ".1"
  Proj2 t -> showTm0 t ++ ".2"
  Let x a t u -> "let " ++ x ++ " : " ++ showTm0 a ++ " = " ++ showTm0 t ++ "; " ++ showTm0 u
  Meta (MetaVar m) -> "?" ++ show m
  InsertedMeta (MetaVar m) _ -> "{?" ++ show m ++ "}"

    
spec :: Spec
spec = do
  describe "Elaboration basics" $ do
    it "elaborates variables" $ do
      result <- elaborationSucceeds "let x : U = U; x"
      result `shouldBe` True
    
    it "elaborates identity function" $ do
      result <- elaborationSucceeds "λ x . x"
      result `shouldBe` True
    
    it "elaborates application" $ do
      result <- elaborationSucceeds "let id : U → U = λ x. x; id U"
      result `shouldBe` True
    
    it "elaborates universe" $ do
      hasType <- checkHasType "U" "U"
      hasType `shouldBe` True

  describe "Elaboration with dependent types" $ do
    it "elaborates Pi types" $ do
      result <- elaborationSucceeds "(x : U) → U"
      result `shouldBe` True
    
    it "elaborates Sigma types" $ do
      result <- elaborationSucceeds "(x : U) × U"
      result `shouldBe` True
    
    it "elaborates pairs" $ do
      result <- elaborationSucceeds "(U, U)"
      result `shouldBe` True
    
    it "elaborates projections" $ do
        let src = unlines [
                "let fst : (A : U) → (B : U) → A × B → A = λ A B p. p.1;",
                "let snd : (A : U) → (B : U) → A × B → B = λ A B p. p.2;", 
                "fst U U (U, U → U)"
                ]
        result <- elaborationSucceeds src
        result `shouldBe` True
        
        -- 可以增加类型检查
        hasType <- checkHasType src "U"
        hasType `shouldBe` True
  
  describe "Elaboration type checking" $ do
    it "checks Pi type correctness" $ do
      hasType <- checkHasType "(x : U) → U" "U"
      hasType `shouldBe` True
    
    it "checks Sigma type correctness" $ do
      hasType <- checkHasType "(x : U) × U" "U"
      hasType `shouldBe` True
    
    it "checks pair type correctness" $ do
      hasType <- checkHasType "(U, U → U)" "U × U"
      hasType `shouldBe` True
  
  describe "Elaboration error cases" $ do
    it "catches unbound variable errors" $ do
      result <- elaborateString "x"
      case result of
        Left (Error _ (UnboundVar _)) -> return ()
        _ -> expectationFailure "Expected unbound variable error"
    
    it "catches type mismatch errors" $ do
      result <- elaborateString "let id : U = λ x. x; id"
      case result of
        Left (Error _ (CantUnify _ _)) -> return ()
        _ -> expectationFailure "Expected type mismatch error"
  
  describe "Elaboration with metavariables (holes)" $ do
    it "elaborates terms with holes" $ do
      result <- elaborationSucceeds "let f : U → U = λ x. _; f"
      result `shouldBe` True
    
    it "creates the correct number of holes" $ do
      _ <- elaborateString "let f : U → U = λ x. _; let g : U = _; (f, g)"
      metas <- getMetas
      length metas `shouldBe` 2