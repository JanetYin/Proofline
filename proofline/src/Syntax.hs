{-# LANGUAGE LambdaCase #-}
module Syntax where

import Common

type Ty = Tm

data Tm
  = Var Ix
  | Lam Name Tm
  | App Tm Tm
  | U
  | Pi Name Ty Ty
  | Sigma Name Tm Tm
  | Pair Tm Tm
  | Proj1 Tm
  | Proj2 Tm
  | Let Name Ty Tm Tm
  | Meta MetaVar
  | InsertedMeta MetaVar [BD] deriving Show
