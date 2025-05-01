{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
module Errors where

import Control.Exception
import Common
import Context
import Syntax

-- ----------
data UnifyError = UnifyError
  deriving (Show, Exception)

data ElabError = NameNotInScope Name | CantUnify Tm Tm | UnboundVar Name
  deriving (Show, Exception)

data Error = Error Context ElabError
  deriving (Show, Exception)
----------------------------------------------------------------------
