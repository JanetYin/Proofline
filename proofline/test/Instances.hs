{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances where

import Test.QuickCheck
import qualified Presyntax as P
import qualified Syntax
import Common

newtype TestName = TestName { unTestName :: Name }
  deriving (Show, Eq)
instance Arbitrary TestName where
  arbitrary = TestName <$> elements ["x", "y", "z", "a", "b", "c", "f", "g", "h"]

instance Arbitrary P.Raw where
  arbitrary = sized genRaw
    where
      genRaw :: Int -> Gen P.Raw
      genRaw 0 = oneof [
          P.RVar . unTestName <$> arbitrary
        , pure P.RU
        , pure P.RHole
        ]
      genRaw n | n > 0 = oneof [
          P.RVar . unTestName <$> arbitrary
        , pure P.RU
        , pure P.RHole
        , P.RLam . unTestName <$> arbitrary <*> genRaw (n `div` 2)
        , P.RApp <$> genRaw (n `div` 2) <*> genRaw (n `div` 2)
        , P.RPi . unTestName <$> arbitrary <*> genRaw (n `div` 2) <*> genRaw (n `div` 2)
        , P.RSigma . unTestName <$> arbitrary <*> genRaw (n `div` 2) <*> genRaw (n `div` 2)
        , P.RPair <$> genRaw (n `div` 2) <*> genRaw (n `div` 2)
        , P.RProj1 <$> genRaw (n `div` 2)
        , P.RProj2 <$> genRaw (n `div` 2)
        ]
      genRaw _ = error "Invalid size for genRaw"

-- Generate valid De Bruijn indices for a given context size
genIx :: Int -> Gen Ix
genIx ctxSize = Ix <$> choose (0, ctxSize - 1)

-- Generate simple core terms with De Bruijn indices
-- contextSize is used to ensure indices are valid
genTm :: Int -> Int -> Gen Syntax.Tm
genTm contextSize size = case size of
  0 -> oneof [
      Syntax.Var <$> genIx contextSize
    , pure Syntax.U
    ]
  n | n > 0 -> oneof [
      Syntax.Var <$> genIx contextSize
    , pure Syntax.U
    , Syntax.Lam . unTestName <$> arbitrary <*> genTm (contextSize + 1) (n `div` 2)
    , Syntax.App <$> genTm contextSize (n `div` 2) <*> genTm contextSize (n `div` 2)
    , Syntax.Pi . unTestName <$> arbitrary <*> genTm contextSize (n `div` 2) <*> genTm (contextSize + 1) (n `div` 2)
    , Syntax.Sigma . unTestName <$> arbitrary <*> genTm contextSize (n `div` 2) <*> genTm (contextSize + 1) (n `div` 2)
    , Syntax.Pair <$> genTm contextSize (n `div` 2) <*> genTm contextSize (n `div` 2)
    , Syntax.Proj1 <$> genTm contextSize (n `div` 2)
    , Syntax.Proj2 <$> genTm contextSize (n `div` 2)
    ]
  _ -> error "Invalid size for genTm"

instance Arbitrary Syntax.Tm where
  arbitrary = sized (genTm 0)