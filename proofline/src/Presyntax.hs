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

instance Eq Raw where
  RVar x == RVar y = x == y
  RU == RU = True
  RLam x t == RLam y u = x == y && t == u
  RApp t1 u1 == RApp t2 u2 = t1 == t2 && u1 == u2
  RPi x a b == RPi y c d = x == y && a == c && b == d
  RSigma x a b == RSigma y c d = x == y && a == c && b == d
  RPair a b == RPair c d = a == c && b == d
  RProj1 t == RProj1 u = t == u
  RProj2 t == RProj2 u = t == u
  RLet x a t u == RLet y b v w = x == y && a == b && t == v && u == w
  RSrcPos _ t == RSrcPos _ u = t == u  -- Compare inner terms, ignoring source positions
  RSrcPos _ t == u = t == u             -- Ignore source position on the left
  t == RSrcPos _ u = t == u             -- Ignore source position on the right
  RHole == RHole = True
  _ == _ = False

-- | Pretty-print raw terms (simple implementation)
showRaw :: Raw -> String
showRaw = go 0
  where
    go :: Int -> Raw -> String
    go _ (RVar x) = x
    go _ RU = "U"
    go p (RLam x t) = parens (p > 0) $ "λ" ++ x ++ ". " ++ go 0 t
    go p (RApp t u) = parens (p > 1) $ go 1 t ++ " " ++ go 2 u
    go p (RPi "_" a b) = parens (p > 0) $ go 2 a ++ " → " ++ go 0 b
    go p (RPi x a b) = parens (p > 0) $ "(" ++ x ++ " : " ++ go 0 a ++ ") → " ++ go 0 b
    go p (RSigma "_" a b) = parens (p > 0) $ go 2 a ++ " × " ++ go 0 b
    go p (RSigma x a b) = parens (p > 0) $ "(" ++ x ++ " : " ++ go 0 a ++ ") × " ++ go 0 b
    go _ (RPair a b) = "(" ++ go 0 a ++ ", " ++ go 0 b ++ ")"
    go _ (RProj1 t) = go 3 t ++ ".1"
    go _ (RProj2 t) = go 3 t ++ ".2"
    go _ (RLet x a t u) = "let " ++ x ++ " : " ++ go 0 a ++ " = " ++ go 0 t ++ "; " ++ go 0 u
    go _ (RSrcPos _ t) = go 0 t  -- Handle source position by printing the inner term
    go _ RHole = "_"
    
    parens :: Bool -> String -> String
    parens True s = "(" ++ s ++ ")"
    parens False s = s