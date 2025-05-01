{-# LANGUAGE LambdaCase #-}
module Presyntax where

import Common

data Raw
  = RVar Name              -- x
  | RLam Name Raw          -- \x. t
  | RApp Raw Raw           -- t u
  | RU                     -- U
  | RPi Name Raw Raw       -- (x : A) -> B
  | RSigma Name Raw Raw    -- (x : A) * B
  | RPair Raw Raw          -- (t, u)
  | RProj1 Raw             -- t.1
  | RProj2 Raw             -- t.2
  | RLet Name Raw Raw Raw  -- let x : A = t; u
  | RSrcPos SourcePos Raw  -- source position for error reporting
  | RHole                  -- _
  deriving Show
