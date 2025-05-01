module ParserSpec (spec) where

import Test.Hspec
-- Import only the instances we need, fixing the "unused imports" warning
import Test.QuickCheck ()
import Control.Exception ()
import Control.Monad.IO.Class ()

import Parser
import Presyntax
import Instances () -- Import Instances to access the Eq instance for Raw if defined there

-- Remove the orphan Eq instance since we'll expect it to be defined in Presyntax.hs or Instances.hs

-- Helper function to parse a string and return result or error in a test context
parseForTest :: String -> IO (Either String Raw)
parseForTest str = parseStringWithErrors str

spec :: Spec
spec = do
  describe "Parser basics" $ do
    it "parses variables" $ do
      result1 <- parseForTest "x"
      result1 `shouldBe` Right (RVar "x")
      
      result2 <- parseForTest "abc"
      result2 `shouldBe` Right (RVar "abc")
      
      result3 <- parseForTest "x'"
      result3 `shouldBe` Right (RVar "x'")
    
    it "parses Universe" $ do
      result <- parseForTest "U"
      result `shouldBe` Right RU
    
    it "parses holes" $ do
      result <- parseForTest "_"
      result `shouldBe` Right RHole
    
    it "handles whitespace" $ do
      result <- parseForTest "   x   "
      result `shouldBe` Right (RVar "x")

  describe "Parser lambda terms" $ do
    it "parses lambda expressions" $ do
      result1 <- parseForTest "λx. x"
      result1 `shouldBe` Right (RLam "x" (RVar "x"))
      
      result2 <- parseForTest "λx. λy. x"
      result2 `shouldBe` Right (RLam "x" (RLam "y" (RVar "x")))
      
      result3 <- parseForTest "\\x. x" 
      result3 `shouldBe` Right (RLam "x" (RVar "x"))
    
    it "parses application" $ do
      result1 <- parseForTest "f x"
      result1 `shouldBe` Right (RApp (RVar "f") (RVar "x"))
      
      result2 <- parseForTest "f x y"
      result2 `shouldBe` Right (RApp (RApp (RVar "f") (RVar "x")) (RVar "y"))
    
    it "handles parentheses" $ do
      result1 <- parseForTest "(x)"
      result1 `shouldBe` Right (RVar "x")
      
      result2 <- parseForTest "f (x y)"
      result2 `shouldBe` Right (RApp (RVar "f") (RApp (RVar "x") (RVar "y")))

  describe "Parser dependent types" $ do
    it "parses Pi types" $ do
      result1 <- parseForTest "(x : A) -> B"
      result1 `shouldBe` Right (RPi "x" (RVar "A") (RVar "B"))
      
      result2 <- parseForTest "A -> B"
      result2 `shouldBe` Right (RPi "_" (RVar "A") (RVar "B"))
    
    it "parses Sigma types" $ do
      result1 <- parseForTest "(x : A) × B"
      result1 `shouldBe` Right (RSigma "x" (RVar "A") (RVar "B"))
      
      result2 <- parseForTest "A × B"
      result2 `shouldBe` Right (RSigma "_" (RVar "A") (RVar "B"))
    
    it "parses pairs" $ do
      result <- parseForTest "(x, y)"
      result `shouldBe` Right (RPair (RVar "x") (RVar "y"))
    
    it "parses projections" $ do
      result1 <- parseForTest "p.1"
      result1 `shouldBe` Right (RProj1 (RVar "p"))
      
      result2 <- parseForTest "p.2"
      result2 `shouldBe` Right (RProj2 (RVar "p"))
      
      result3 <- parseForTest "(x, y).1"
      result3 `shouldBe` Right (RProj1 (RPair (RVar "x") (RVar "y")))

  describe "Parser let expressions" $ do
    it "parses let bindings" $ do
      result1 <- parseForTest "let x = y; z"
      result1 `shouldBe` Right (RLet "x" RHole (RVar "y") (RVar "z"))
      
      result2 <- parseForTest "let x : A = y; z"
      result2 `shouldBe` Right (RLet "x" (RVar "A") (RVar "y") (RVar "z"))

  describe "Parser precedence and associativity" $ do
    it "handles application precedence" $ do
      result <- parseForTest "f x y"
      result `shouldBe` Right (RApp (RApp (RVar "f") (RVar "x")) (RVar "y"))
    
    it "handles arrow precedence" $ do
      result <- parseForTest "A -> B -> C"
      result `shouldBe` Right (RPi "_" (RVar "A") (RPi "_" (RVar "B") (RVar "C")))
    
    it "handles product precedence" $ do
      result <- parseForTest "A × B × C"
      result `shouldBe` Right (RSigma "_" (RVar "A") (RSigma "_" (RVar "B") (RVar "C")))
    
    it "arrow binds looser than product" $ do
      result <- parseForTest "A × B -> C"
      result `shouldBe` Right (RPi "_" (RSigma "_" (RVar "A") (RVar "B")) (RVar "C"))

  describe "Parser comments" $ do
    it "handles line comments" $ do
      result <- parseForTest "x -- this is a comment\ny"
      result `shouldBe` Right (RApp (RVar "x") (RVar "y"))
    
    it "handles block comments" $ do
      result <- parseForTest "x {- this is\na block comment -} y"
      result `shouldBe` Right (RApp (RVar "x") (RVar "y"))

  describe "Parser error reporting" $ do
    it "reports unbalanced parentheses" $ do
      result <- parseForTest "(x"
      case result of
        Left err -> err `shouldContain` "unexpected end of input"
        Right _ -> expectationFailure "Expected parse error"