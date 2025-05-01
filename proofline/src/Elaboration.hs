{-# LANGUAGE LambdaCase #-}
module Elaboration (check, infer) where

import Control.Exception
import Control.Monad()  -- Changed to only import instances
import Data.IORef

import qualified Data.IntMap as IM

import Common
import Context
import Errors
import Evaluation
import Metacontext
import Syntax
import Unification
import Value

import Presyntax (Raw(..))


-- Elaboration
--------------------------------------------------------------------------------
freshMeta :: Context -> VTy -> IO Tm
freshMeta cxt expType = do  
  m <- readIORef nextMeta
  writeIORef nextMeta $! (m + 1)
  modifyIORef' mcxt $ IM.insert m (Unsolved (types cxt) expType)
  
  pure $ InsertedMeta (MetaVar m) (bds cxt)

unifyCatch :: Context -> Value -> Value -> IO ()
unifyCatch cxt t t' =
  unify (lvl cxt) t t'
  `catch` \UnifyError ->
    throwIO $ Error cxt $ CantUnify (quote (lvl cxt) t) (quote (lvl cxt) t')

check :: Context -> Raw -> VTy -> IO Tm
check cxt t a = case (t, force a) of 
  (RSrcPos srcPos term, aVal) ->  
    check (cxt {pos = srcPos}) term aVal 
  (RLam x term, VPi _ aVal bVal) -> do  
    Lam x <$> check (bind cxt x aVal) term (evalClosure bVal (var (lvl cxt)))
  (RLet x aRaw tRaw u, e) -> do  
    a' <- check cxt aRaw VU
    let aVal = eval (env cxt) a'
    t' <- check cxt tRaw aVal
    let tVal = eval (env cxt) t'
    u' <- check (define cxt x tVal aVal) u e
    pure (Let x a' t' u')
  (RPair tRaw uRaw, VSigma _ aVal bVal) -> do  
    t' <- check cxt tRaw aVal
    u' <- check cxt uRaw (evalClosure bVal (eval (env cxt) t'))
    pure (Pair t' u')
  (RHole, aVal) -> do  
    freshMeta cxt aVal
  (term, expected) -> do  
    (tm, inferred) <- infer cxt term
    unifyCatch cxt expected inferred
    pure tm

lookupVar :: Context -> Name -> Maybe (Ix, VTy)
lookupVar cxt x = go 0 (types cxt)
  where
    go _ [] = Nothing
    go i ((y, ty):tys)
      | x == y    = Just (Ix i, ty)
      | otherwise = go (i+1) tys

infer :: Context -> Raw -> IO (Tm, VTy)
infer cxt = \case
  RSrcPos srcPos term ->  
    do
      infer (cxt {pos = srcPos}) term

  RVar x -> do
    case lookupVar cxt x of
      Just (ix, ty) -> return (Var ix, ty)
      Nothing -> throwIO $ Error cxt $ UnboundVar x

  RLam x term -> do  
    a <- eval (env cxt) <$> freshMeta cxt VU  
    (tm, b) <- infer (bind cxt x a) term  
    pure (Lam x tm, VPi x a $ closeVal cxt b)

  RApp term uRaw -> do  
    (tm, tty) <- infer cxt term  

    (aVal, bVal) <- case force tty of  
      VPi _ aVal bVal -> pure (aVal, bVal)  
      ttyForced -> do  
        aVal <- eval (env cxt) <$> freshMeta cxt VU
        bVal <- Closure (env cxt) <$> freshMeta (bind cxt "x" aVal) VU
        unifyCatch cxt (VPi "x" aVal bVal) ttyForced
        pure (aVal, bVal)

    u <- check cxt uRaw aVal
    pure (App tm u, evalClosure bVal (eval (env cxt) u))

  RU ->
    pure (U, VU)

  RPi x aRaw bRaw -> do  
    a' <- check cxt aRaw VU
    b' <- check (bind cxt x (eval (env cxt) a')) bRaw VU
    pure (Pi x a' b', VU)

  RLet x aRaw tRaw uRaw -> do  
    a' <- check cxt aRaw VU
    let ~va = eval (env cxt) a'
    t' <- check cxt tRaw va
    let ~vt = eval (env cxt) t'
    (u', bVal) <- infer (define cxt x vt va) uRaw  
    pure (Let x a' t' u', bVal)

  RHole -> do
    a <- eval (env cxt) <$> freshMeta cxt VU  
    t <- freshMeta cxt a  
    pure (t, a)
    
  RSigma x aRaw bRaw -> do  
    a' <- check cxt aRaw VU
    b' <- check (bind cxt x (eval (env cxt) a')) bRaw VU
    pure (Sigma x a' b', VU)
    
  RPair tRaw uRaw -> do  
    (t', aVal) <- infer cxt tRaw  
    (u', bVal) <- infer cxt uRaw  
    pure (Pair t' u', VSigma "_" aVal $ closeVal cxt bVal)
    
  RProj1 term -> do  
    (tm, tty) <- infer cxt term  

    (aVal, _) <- case force tty of  
      VSigma _ aVal bVal -> pure (aVal, bVal)  
      ttyForced -> do  
        aVal <- eval (env cxt) <$> freshMeta cxt VU
        bVal <- Closure (env cxt) <$> freshMeta (bind cxt "x" aVal) VU
        unifyCatch cxt (VSigma "x" aVal bVal) ttyForced
        pure (aVal, bVal)

    pure (Proj1 tm, aVal)
    
  RProj2 term -> do  
    (tm, tty) <- infer cxt term  
    
    (_, bVal) <- case force tty of  
      VSigma _ aVal bVal -> pure (aVal, bVal)  
      ttyForced -> do  
        aVal <- eval (env cxt) <$> freshMeta cxt VU
        bVal <- Closure (env cxt) <$> freshMeta (bind cxt "x" aVal) VU
        unifyCatch cxt (VSigma "x" aVal bVal) ttyForced
        pure (aVal, bVal)
    
    pure (Proj2 tm, evalClosure bVal (proj1 (eval (env cxt) tm)))