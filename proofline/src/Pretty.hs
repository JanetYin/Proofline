{-# LANGUAGE LambdaCase #-}
module Pretty (prettyTm, showTm0) where

import Common
import Syntax

showTm0 :: Tm -> String
showTm0 t = prettyTm 0 [] t []

-- Compute a fresh name for a bound variable
fresh :: Name -> [Name] -> Name
fresh x xs | x `elem` xs = fresh (x ++ "'") xs
           | otherwise   = x

-- Pretty printing names with De Bruijn indices, internal representation.
-- `depth` counts the number of binders from the root of the term.
-- `ns` are the names from the context.
prettyTm :: Int -> [Name] -> Tm -> ShowS
prettyTm prec = go prec where
    
    par :: Bool -> ShowS -> ShowS
    par b s = if b then showParen True s else s
    
    -- Removed unused parameter 'p'
    goArg :: [Name] -> Tm -> ShowS
    goArg ns t = prettyTm 10 ns t
    
    go :: Int -> [Name] -> Tm -> ShowS
    go p ns = \case
      Var (Ix x) ->
        case ns of
          [] -> showString "<var " . shows x . showString ">"
          _  -> case drop x ns of
                 []    -> showString "<var " . shows x . showString ">"
                 (n:_) -> showString n
      
      Lam x t -> par (p > 0) $ do
        let x' = fresh x ns
        showString "λ" . showString x' . showString ". " . go 0 (x':ns) t
      
      App t u -> par (p > 9) $
        go 9 ns t . showString " " . goArg ns u
      
      U -> showString "U"
      
      Pi "_" a b -> par (p > 0) $
        go 5 ns a . showString " → " . go 0 ("_":ns) b
      
      Pi x a b -> par (p > 0) $ do
        let x' = fresh x ns
        showString "(" . showString x' . showString " : " . go 0 ns a . showString ") → " . go 0 (x':ns) b
      
      Sigma "_" a b -> par (p > 0) $
        go 5 ns a . showString " × " . go 0 ("_":ns) b
      
      Sigma x a b -> par (p > 0) $ do
        let x' = fresh x ns
        showString "(" . showString x' . showString " : " . go 0 ns a . showString ") × " . go 0 (x':ns) b
      
      Pair t u -> par (p > 0) $
        showString "(" . go 0 ns t . showString ", " . go 0 ns u . showString ")"
      
      Proj1 t -> par (p > 10) $
        go 10 ns t . showString ".1"
      
      Proj2 t -> par (p > 10) $
        go 10 ns t . showString ".2"
      
      Let x a t u -> par (p > 0) $ do
        let x' = fresh x ns
        showString "let " . showString x' . showString " : " . go 0 ns a .
          showString "\n = " . go 0 ns t . showString ";\n\n " . go 0 (x':ns) u
      
      Meta (MetaVar m) ->
        showString "?" . shows m
      
      InsertedMeta (MetaVar m) _ ->
        showString "{?" . shows m . showString "}"