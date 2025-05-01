{-# LANGUAGE LambdaCase #-}
module Context where

import Common
import Value
import Syntax
import Evaluation (quote)
import qualified Pretty

-- Elaboration context
--------------------------------------------------------------------------------

-- Define Types directly in Context
type Types = [(Name, VTy)]

data Context = Context {           -- used for:
                           -----------------------------------
    env   :: Env           -- evaluation
  , lvl   :: Lvl           -- unification
  , types :: Types         -- raw name lookup, pretty printing
  , bds   :: [BD]          -- fresh meta creation
  , pos   :: SourcePos     -- error reporting
  }

contextNames :: Context -> [Name]
contextNames = fmap fst . types

showVal :: Context -> Value -> String
showVal cxt v =
  Pretty.prettyTm 0 (contextNames cxt) (quote (lvl cxt) v) []

showTm :: Context -> Tm -> String
showTm cxt t = Pretty.prettyTm 0 (contextNames cxt) t []

instance Show Context where
  show = show . contextNames

emptyContext :: SourcePos -> Context
emptyContext = Context [] 0 [] []

-- | Extend Context with a bound variable.
bind :: Context -> Name -> VTy -> Context
bind (Context env' lvl' types' bds' pos') x ~a =
  Context (env' :> var lvl') (lvl' + 1) (types' :> (x, a)) (bds' :> Bound) pos'

-- | Extend Context with a definition.
define :: Context -> Name -> Value -> VTy -> Context
define (Context env' lvl' types' bds' pos') x ~t ~a  =
  Context (env' :> t) (lvl' + 1) (types' :> (x, a)) (bds' :> Defined) pos'
  
-- | closeVal : (Γ : Con) → Val (Γ, x : A) B → Closure Γ A B
closeVal :: Context -> Value -> Closure
closeVal cxt t = Closure (env cxt) (quote (lvl cxt + 1) t)