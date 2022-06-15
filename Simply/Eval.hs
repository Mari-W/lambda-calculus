module Simply.Eval where

import Simply.Ast
  ( DecExpr (..),
    Ident,
    InfExpr (..),
    Neutral (..),
    Value (..),
  )

type Ctx = [Value]

vFree :: Ident -> Value
vFree n = VNeutral (NFree n)

eval :: InfExpr -> Value
eval inf = evalInf inf []

evalInf :: InfExpr -> Ctx -> Value
evalInf (Ann dec _) ctx = evalDec dec ctx
evalInf (Free n) _ = VNeutral (NFree n)
evalInf (Bound i) ctx = ctx !! i
evalInf (App lam app) ctx = vApp (evalInf lam ctx) (evalDec app ctx)

vApp :: Value -> Value -> Value
vApp (VClos f) v = f v
vApp (VNeutral n) v = VNeutral (NApp n v)

evalDec :: DecExpr -> Ctx -> Value
evalDec (InfExpr i) ctx = evalInf i ctx
evalDec (Lam e) ctx = VClos (\x -> evalDec e (x : ctx))