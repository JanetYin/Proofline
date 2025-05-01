{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}

module Value (
  -- Export all the types and constructors
  Value(..), Var(..), Spine(..),
  Env, Closure(..), VTy,
  var, meta
) where

import Common
import Syntax (Tm)

type Env = [Value]
data Closure = Closure Env Tm deriving Show  
type VTy = Value

data Var
  = Rigid Lvl
  | Flex MetaVar
  deriving stock (Show, Eq)

data Value
  = VNeutral Var Spine 
  | VU 
  | VLam Name Closure
  | VPi Name VTy Closure
  | VSigma Name VTy Closure
  | VPair Value Value 
  deriving Show

data Spine
  = Nil 
  | NApp Spine Value
  | NProj1 Spine
  | NProj2 Spine 
  deriving Show

var :: Lvl -> Value
var lx = VNeutral (Rigid lx) Nil

meta :: MetaVar -> Value
meta mx = VNeutral (Flex mx) Nil