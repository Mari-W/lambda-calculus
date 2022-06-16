module Dependently.Eval where

import Dependently.Ast
  ( ChkExpr (..),
    Ident,
    SynExpr (..),
    Neutral (..),
    Value (..),
  )

type Ctx = [Value]

eval :: SynExpr -> Value
eval syn = evalSyn syn []

evalSyn :: SynExpr -> Ctx -> Value
evalSyn Star ctx = VStar
evalSyn (Pi dom range) ctx = VPi (evalChk dom ctx) (\x -> evalChk range (x : ctx))
evalSyn (Ann chk _) ctx = evalChk chk ctx
evalSyn (Free n) ctx = VNeutral (NFree n)
evalSyn (Bound i) ctx = ctx !! i
evalSyn (App e e') ctx = do
  let e'' = evalSyn e ctx
  case e'' of
    VClos f -> f (evalChk e' ctx)
    VNeutral n -> VNeutral (NApp n (evalChk e' ctx))
    _ -> error "unreachable: should only be a closure or neutral if type checked"

evalChk :: ChkExpr -> Ctx -> Value
evalChk (SynExpr i) ctx = evalSyn i ctx
evalChk (Lam e) ctx = VClos (\x -> evalChk e (x : ctx))