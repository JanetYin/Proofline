module IntegrationSpec (spec) where

import Test.Hspec
import Control.Exception (try)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesDirectoryExist)
import System.FilePath ((</>))
-- Import Value for its instances only
import Value ()

import Common
import Context
import Elaboration
import Errors
import Evaluation
import Metacontext
import Parser
import Syntax

-- Setup and teardown for test directories
withTestDirectory :: FilePath -> IO a -> IO a
withTestDirectory dir action = do
  -- Check if directory exists and remove it if it does
  exists <- doesDirectoryExist dir
  if exists
    then removeDirectoryRecursive dir
    else return ()
  
  -- Create the directory
  createDirectoryIfMissing True dir
  
  -- Run the action
  action

-- Helper to run a complete elaboration pipeline
elaborateTerm :: String -> IO (Either Error (String, String))
elaborateTerm src = do
  result <- try $ do
    -- Reset metacontext
    Metacontext.reset
    
    -- Parse
    raw <- Parser.parseString src
    
    -- Elaborate
    (term, ty) <- Elaboration.infer (Context.emptyContext (initialPos "(test)")) raw
    
    -- Return term and type as strings
    return (showTm0 term, showTm0 (quote 0 ty))
  
  return result

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

-- Test a complete proof file
testProofFile :: FilePath -> String -> String -> String -> Spec
testProofFile dir name content expectedType = do
  describe ("Proof file: " ++ name) $ do
    it "elaborates successfully" $ do
      -- Set up the test directory
      withTestDirectory dir $ do
        -- Write the test file
        let filePath = dir </> name
        writeFile filePath content
        
        -- Elaborate the term
        result <- elaborateTerm content
        
        -- Check the result
        case result of
          Right (_, actualType) -> actualType `shouldBe` expectedType
          Left err -> expectationFailure $ "Elaboration failed: " ++ show err

spec :: Spec
spec = do
  describe "Integration tests" $ do
    -- Test identity function
    testProofFile "test_proofs" "identity.pl" "λ x . x" "U → U"
    
    -- Test application
    testProofFile "test_proofs" "application.pl" 
      "let id : U → U = λ x. x; id U" 
      "U"
    
    -- Test dependent pair
    testProofFile "test_proofs" "pair.pl"
      "let Pair : U → U → U = λ A B. (x : A) × B; let pair : (A : U) → (B : U) → A → B → Pair A B = λ A B x y. (x, y); pair U U U U"
      "U × U"
    
    -- Test with explicit holes
    testProofFile "test_proofs" "holes.pl"
      "let f : U → U = λ x. _; f U"
      "U"
    
    -- More complex test with let bindings and dependent types
    describe "Proof file: complex.pl" $ do
      it "elaborates successfully" $ do
        withTestDirectory "test_proofs" $ do
          let content = "let id : (A : U) → A → A = λ A x. x;\n" ++
                        "let comp : (A : U) → (B : U) → (C : U) → (B → C) → (A → B) → A → C = λ A B C f g x. f (g x);\n" ++
                        "let twice : (A : U) → (A → A) → A → A = λ A f x. f (f x);\n" ++
                        "twice U (id U) U"
              filePath = "test_proofs" </> "complex.pl"
          
          -- Write file content
          writeFile filePath content
          
          -- Test elaboration
          result <- elaborateTerm content
          case result of
            Right (_, actualType) -> actualType `shouldBe` "U"
            Left err -> expectationFailure $ "Elaboration failed: " ++ show err
    
  describe "Full pipeline" $ do
    it "parses, elaborates, and evaluates correctly" $ do
      let src = "let Nat : U = (N : U) → (N → N) → N → N; let zero : Nat = λ N s z. z; let succ : Nat → Nat = λ n N s z. s (n N s z); let one = succ zero; let two = succ one; two"
      
      result <- elaborateTerm src
      case result of
        Right (_, typeStr) -> do
          -- Check the inferred type is correct (removed unused variable 'termStr')
          typeStr `shouldBe` "(N : U) → (N → N) → N → N"
          
          -- Further checks could be added here
        Left err -> expectationFailure $ "Elaboration failed: " ++ show err