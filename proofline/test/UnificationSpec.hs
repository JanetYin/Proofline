module UnificationSpec (spec) where

import Test.Hspec
import Control.Exception (try)

-- Direct imports now that modules are at the top level
import Common
import Context
import Errors
import Evaluation
import Metacontext
import Parser
import Elaboration
import Unification
import Value

-- Helper to create a value from a string
parseAndEval :: String -> IO Value
parseAndEval src = do
  raw <- Parser.parseString src
  (tm, _) <- Elaboration.infer (Context.emptyContext (initialPos "(test)")) raw
  return $ eval [] tm

-- Helper to test unification
testUnify :: String -> String -> IO Bool
testUnify s1 s2 = do
  Metacontext.reset
  v1 <- parseAndEval s1
  v2 <- parseAndEval s2
  result <- try $ unify 0 v1 v2
  return $ case result of
    Right () -> True
    Left (UnifyError) -> False

spec :: Spec
spec = do
  describe "Unification basics" $ do
    it "unifies identical terms" $ do
      result <- testUnify "U" "U"
      result `shouldBe` True
      
      result2 <- testUnify "λ x. x" "λ y. y"
      result2 `shouldBe` True
    
    it "unifies beta-equivalent terms" $ do
      result <- testUnify "(λ x. x) U" "U"
      result `shouldBe` True
    
    it "fails on non-unifiable terms" $ do
      result <- testUnify "U" "U → U"
      result `shouldBe` False
  
--   describe "Unification with metavariables" $ do
--     it "solves simple metavariables" $ do
--         Metacontext.reset
        
--         let src = unlines [
--                 "let id : U → U = λ x. x;",
--                 "let f : U → U = λ x. _;",  
--                 "let test = f U;",           
--                 "let test2 = id U"           
--                 ]
        
--         raw <- Parser.parseString src
--         (_, _) <- Elaboration.infer (Context.emptyContext (initialPos "(test)")) raw
        
--         metas <- getMetas
--         length metas `shouldBeGreaterThan` 0
  
  describe "Unification with dependent types" $ do
    it "unifies in Pi types" $ do
      result <- testUnify "(x : U) → U" "(y : U) → U"
      result `shouldBe` True
    
    it "unifies in Sigma types" $ do
      result <- testUnify "(x : U) × U" "(y : U) × U"
      result `shouldBe` True
    
    it "unifies in pairs" $ do
      result <- testUnify "(U, U)" "(U, U -> U)"
      result `shouldBe` False