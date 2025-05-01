{-# LANGUAGE LambdaCase #-}
module Evaluation where

import Common
import Metacontext
import Syntax
import Value
import Pretty

showVal0 :: Value -> String
showVal0 v = prettyTm 0 [] (quote 0 v) []

evalClosure :: Closure -> Value -> Value
evalClosure (Closure env t) ~u = eval (env :> u) t

openClosure :: Lvl -> Closure -> Value
openClosure lvl closure = evalClosure closure (var lvl)

appSpine :: Value -> Spine -> Value 
appSpine t = \case 
    Nil -> t 
    NApp spine u -> appValue (appSpine t spine) u
    NProj1 sp -> proj1 (appSpine t sp) 
    NProj2 sp -> proj2 (appSpine t sp) 

proj1 :: Value -> Value
proj1 = \case
  VNeutral x spine -> VNeutral x $ NProj1 spine
  VPair t _ -> t
  _ -> error "bug"

proj2 :: Value -> Value
proj2 = \case
  VNeutral x spine -> VNeutral x $ NProj2 spine
  VPair _ u -> u
  _ -> error "bug"

vMeta :: MetaVar -> Value
vMeta m = case lookupMeta m of
  Solved v -> v
  Unsolved _ _ -> meta m

appValue :: Value -> Value -> Value
appValue t ~u = case t of
  VNeutral x spine -> VNeutral x (NApp spine u)
  VLam _ closure ->  evalClosure closure u
  _ -> error "bug"

vAppBDs :: Env -> Value -> [BD] -> Value
vAppBDs env ~v bds = case (env, bds) of
  ([]       , []            ) -> v
  (env' :> t , bds' :> Bound  ) -> vAppBDs env' v bds' `appValue` t
  (env' :> _ , bds' :> Defined) -> vAppBDs env' v bds'
  _                           -> error "impossible"

eval :: Env -> Tm -> Value
eval env = \case 
  Var x -> env !! unIx x 
  Pi x a b -> VPi x (eval env a) (Closure env b)
  Lam x body -> VLam x (Closure env body)
  App t u -> appValue (eval env t) (eval env u)
  Sigma x l r -> VSigma x (eval env l) (Closure env r)
  Pair l r -> VPair (eval env l) (eval env r)
  Proj1 t -> proj1 (eval env t)
  Proj2 t -> proj2 (eval env t)
  Let _ _ t u -> eval (env :> eval env t ) u
  U -> VU 
  Meta m -> vMeta m
  InsertedMeta m bds -> vAppBDs env (vMeta m) bds

force :: Value -> Value 
force = \case 
    VNeutral (Flex m) sp | Solved t <- lookupMeta m -> force (appSpine t sp)
    t -> t

lvl2Ix :: Lvl -> Lvl -> Ix
lvl2Ix (Lvl l) (Lvl x) = Ix (l - x - 1)

quoteSp :: Lvl -> Tm -> Spine -> Tm
quoteSp l t = \case
    Nil -> t 
    NApp sp u -> App (quoteSp l t sp) (quote l u)
    NProj1 sp -> Proj1 (quoteSp l t sp) 
    NProj2 sp -> Proj2 (quoteSp l t sp) 

quote :: Lvl -> Value -> Tm
quote l t = case force t of
  VNeutral (Rigid x) spine -> quoteSp l (Var (lvl2Ix l x)) spine
  VNeutral (Flex m) spine ->  quoteSp l (Meta m) spine
  VLam x closure -> Lam x (quote (l + 1) (evalClosure closure (var l)))
  VPi  x a b  -> Pi x (quote l a) (quote (l + 1) (evalClosure b (var l)))
  VU          -> U
  VSigma x a b -> Sigma x (quote l a) (quote (l + 1) (evalClosure b (var l)))
  VPair t' u -> Pair (quote l t') (quote l u)

nf :: Env -> Tm -> Tm
nf env t = quote (Lvl (length env)) (eval env t)