module Simply.Eval where

import Simply.Ast
  ( ChkExpr (..),
    Ident,
    SynExpr (..),
    Neutral (..),
    Value (..),
  )

type Ctx = [Value]

vFree :: Ident -> Value
vFree n = VNeutral (NFree n)

eval :: SynExpr -> Value
eval syn = evalSyn syn []

evalSyn :: SynExpr -> Ctx -> Value
evalSyn (Ann chk _) ctx = evalChk chk ctx
evalSyn (Free n) _ = VNeutral (NFree n)
evalSyn (Bound i) ctx = ctx !! i
evalSyn (App lam app) ctx = vApp (evalSyn lam ctx) (evalChk app ctx)

vApp :: Value -> Value -> Value
vApp (VClos f) v = f v
vApp (VNeutral n) v = VNeutral (NApp n v)

evalChk :: ChkExpr -> Ctx -> Value
evalChk (SynExpr i) ctx = evalSyn i ctx
evalChk (Lam e) ctx = VClos (\x -> evalChk e (x : ctx))