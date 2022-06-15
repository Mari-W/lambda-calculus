module Dependently.Eval where

import Dependently.Ast
  ( DecExpr (..),
    Ident,
    InfExpr (..),
    Neutral (..),
    Value (..),
  )

type Ctx = [Value]

eval :: InfExpr -> Value
eval inf = evalInf inf []

evalInf :: InfExpr -> Ctx -> Value
evalInf Star ctx = VStar
evalInf (Pi dom dec) ctx = VPi (evalDec dom ctx) (\x -> evalDec dec (x : ctx))
evalInf (Ann dec _) ctx = evalDec dec ctx
evalInf (Free n) _ = VNeutral (NFree n)
evalInf (Bound i) ctx = ctx !! i
evalInf (App lam app) ctx = do
  let e = evalInf lam ctx
  case e of
    VClos f -> f (evalDec app ctx)
    VNeutral n -> VNeutral (NApp n (evalDec app ctx))
    _ -> error "unreachable: should only be a closure or neutral if type checked"

evalDec :: DecExpr -> Ctx -> Value
evalDec (InfExpr i) ctx = evalInf i ctx
evalDec (Lam e) ctx = VClos (\x -> evalDec e (x : ctx))