{-# options_ghc -Wno-missing-pattern-synonym-signatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
module Common
  (
      module Common
    , SourcePos(..)
    , Pos
    , unPos
    , initialPos
  ) where

import Text.Megaparsec

type Name = String

data BD = Bound | Defined deriving Show

-- | De Bruijn index.
newtype Ix  = Ix {unIx :: Int} deriving (Eq, Show, Num) via Int

-- | De Bruijn level.
newtype Lvl = Lvl {unLvl :: Int} deriving (Eq, Ord, Show, Num) via Int

newtype MetaVar = MetaVar {unMetaVar :: Int} deriving (Eq, Show, Num) via Int

-- Snoc --------------------------------------------------------------------------------
infixl 4 :>
pattern xs :> x <- x:xs where
   (:>) xs ~x = x:xs
{-# complete (:>), [] #-}
