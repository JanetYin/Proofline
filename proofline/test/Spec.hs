module Main where

import Test.Hspec
import qualified ParserSpec
import qualified ElaborationSpec
import qualified UnificationSpec  
import qualified APISpec
import qualified IntegrationSpec

main :: IO ()
main = hspec $ do
  describe "Parser" ParserSpec.spec
  describe "Elaboration" ElaborationSpec.spec
  describe "Unification" UnificationSpec.spec  
  describe "API" APISpec.spec 
  -- describe "Integration" IntegrationSpec.spec  -- 暂时注释掉