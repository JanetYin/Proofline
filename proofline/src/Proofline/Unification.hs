{-# LANGUAGE LambdaCase #-}
module Unification (unify) where

import Control.Exception
import Data.IORef

import qualified Data.IntMap as IM

import Common
import Errors
import Evaluation
import Metacontext
import Syntax
import Value

-- Unification
--------------------------------------------------------------------------------
                                           -- Context has a (bds :: Cxt -> [BD]) field
data PartialRenaming = PRen {
    domain :: Lvl                -- size of Γ
  , codomain :: Lvl              -- size of Δ
  , rename :: IM.IntMap Lvl }    -- mapping from Δ vars to Γ vars

lift :: PartialRenaming -> PartialRenaming
lift (PRen domain codomain rename) =
  PRen (domain + 1) (codomain + 1) (IM.insert (unLvl codomain) domain rename)

invert :: Lvl -> Spine -> IO PartialRenaming
invert gamma sp = do 
  let  go :: Spine -> IO (Lvl, IM.IntMap Lvl)
       go Nil = pure (0, mempty)
       go (NApp sp v) = do 
        (domain,rename) <- go sp 
        case force v of 
          VNeutral (Rigid lv) Nil | IM.notMember (unLvl lv) rename -> do 
            pure (domain+1, IM.insert (unLvl lv) domain rename)
          _ -> throwIO UnifyError
  (domain, ren) <- go sp
  pure $ PRen domain gamma ren

renaming :: MetaVar -> PartialRenaming -> Value -> IO Tm
renaming m pren v = go pren v where
  goSp :: PartialRenaming -> Tm -> Spine -> IO Tm
  goSp pren t Nil = pure t 
  goSp pren t (NApp spine u) = App <$> goSp pren t spine <*> go pren u

  go :: PartialRenaming -> Value -> IO Tm
  go pren t = case force t of 
    VNeutral (Rigid lv) sp -> case IM.lookup (unLvl lv) (rename pren) of 
      Nothing -> throwIO UnifyError 
      Just x' -> goSp pren (Var $ lvl2Ix (domain pren) x') sp
    VNeutral (Flex m') sp | m == m' -> throwIO UnifyError 
                          | otherwise -> goSp pren (Meta m') sp
                             
    VLam x t -> Lam x <$> go (lift pren) (evalClosure t (var (codomain pren)))
    VPi x a b -> Pi x <$> go pren a <*> go (lift pren) (evalClosure b (var (codomain pren)))
    VU -> pure U
    -- Todo : fix unification for Sigma type
    -- VSigma x a b -> Sigma x <$> go pren a <*> go (lift pren) (evalClosure b (var (codomain pren)))
    -- VPair t u -> Pair <$> go pren t <*> go pren u
  -- rename :: Level -> Renaming -> Maybe Level
  -- rename (Level n) (Renaming _ m) = m ^. at n

{-
Wrap a term in lambdas.
-}
lams :: Lvl -> Tm -> Tm
lams l = go 0 where
  go x t | x == l = t
  go x t = Lam ("x"++show (x+1)) $ go (x + 1) t

--       Γ      ?α         sp       rhs
solve :: Lvl -> MetaVar -> Spine -> Value -> IO ()
solve gamma m sp rhs = do
    
  pren <- invert gamma sp

  rhs  <- renaming m pren rhs

  let solution = eval [] $ lams (domain pren) rhs

  modifyIORef' mcxt $ IM.insert (unMetaVar m) (Solved solution)

unifySp :: Lvl -> Spine -> Spine -> IO () 
unifySp l sp sp' = case (sp, sp') of 
   (Nil ,Nil) -> pure () 
   (NApp sp v, NApp sp' v') -> 
      unifySp l sp sp' >> unify l v v'
   (NProj1 sp , NProj1 sp') -> unifySp l sp sp'
   (NProj2 sp , NProj2 sp') -> unifySp l sp sp'
   _ ->  throwIO UnifyError
  
unify :: Lvl -> Value -> Value -> IO ()
unify l t u = case (force t, force u) of
  (VLam _ t   , VLam _ t'    ) ->  unify (l + 1) (openClosure l t) (openClosure l t')
  (VNeutral x sp, VLam _ t'    ) -> unify (l + 1) (VNeutral x $ NApp sp (var l)) (openClosure l t')
  (VLam _ t   , VNeutral x sp  ) -> unify (l + 1) (openClosure l t) (VNeutral x $ NApp sp (var l))
  (VU         , VU           ) -> pure ()
  (VPi x a b  , VPi x' a' b' ) -> unify l a a' >> unify (l + 1) (openClosure l b) (openClosure l b')
  (VSigma _ a t, VSigma _ a' t') -> unify l a a' >> unify (l + 1) (openClosure l t) (openClosure l t')
  (VPair t u  , VPair t' u'    ) -> unify l t t' >> unify l u u'
  (VPair t u, VNeutral x sp    ) -> unify l t (VNeutral x $ NProj1 sp) >> unify l u (VNeutral x $ NProj2 sp)
  (VNeutral x sp   , VPair t u  ) ->  unify l (VNeutral x $ NProj1 sp) t >> unify l (VNeutral x $ NProj2 sp) u
  (VNeutral x sp, VNeutral x' sp') | x == x' -> do 
    unifySp l sp sp'
  (VNeutral (Flex m) sp, t') -> solve l m sp t'
  (t, VNeutral (Flex m') sp') -> do 
    solve l m' sp' t 
  _ -> throwIO UnifyError