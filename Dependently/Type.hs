module Dependently.Type where

import Control.Monad (unless)
import Dependently.Ast
  ( Binders,
    ChkExpr (..),
    Ident (Local),
    SynExpr (..),
    Neutral (NFree),
    Result,
    Value (..),
  )
import Dependently.Eval (evalChk)
import Dependently.Quote (quote)

type Env = [(Ident, Value)]

raise :: String -> Result r
raise = Left

typeOf :: Env -> SynExpr -> Result Value
typeOf = typeSyn 0

typeSyn :: Binders -> Env -> SynExpr -> Result Value
typeSyn i env Star = return VStar
typeSyn i env (Pi dom range) = do
  typeChk i env dom VStar
  typeChk (i + 1) ((Local i, evalChk dom []) : env) (subst (Free (Local i)) range) VStar
  return VStar
typeSyn i env (Ann e p) = do
  typeChk i env p VStar
  let v = evalChk p []
  typeChk i env e v
  return v
typeSyn i env (Free ident) = case lookup ident env of
  Just v -> return v
  _ -> raise ("unresolved: unknown ident " ++ show ident)
typeSyn i env (App e e') =
  do
    w <- typeSyn i env e
    case w of
      VPi v f -> do
        typeChk i env e' v
        return (f (evalChk e' []))
      _ -> raise ("type mismatch: expected dependent function, got " ++ show (quote w))
typeSyn i env (Bound j) = raise ("unresolved: unbound variable " ++ show j ++ " should have been substituted by now")

typeChk :: Binders -> Env -> ChkExpr -> Value -> Result ()
typeChk i env (SynExpr syn) v = do
  w <- typeSyn i env syn
  unless (quote v == quote w) (raise ("type mismatch: expected " ++ show (quote v) ++ ", got " ++ show (quote w)))
typeChk i env (Lam lam) (VPi v f) = typeChk (i + 1) ((Local i, v) : env) (subst (Free (Local i)) lam) (f (VNeutral (NFree (Local i))))
typeChk i env (Lam lam) v = raise ("type mismatch: expected function, got " ++ show (quote v))

subst :: SynExpr -> ChkExpr -> ChkExpr
subst = substChk 0

substChk :: Binders -> SynExpr -> ChkExpr -> ChkExpr
substChk i f (SynExpr syn) = SynExpr (substSyn i f syn)
substChk i f (Lam lam) = Lam (substChk (i + 1) f lam)

substSyn :: Binders -> SynExpr -> SynExpr -> SynExpr
substSyn i f Star = Star
substSyn i f (Pi v b) = Pi (substChk i f v) (substChk (i + 1) f b)
substSyn i f (Ann chk v) = Ann (substChk i f chk) (substChk i f v)
substSyn i f (Bound j) = if i == j then f else Bound j
substSyn i f (Free id) = Free id
substSyn i f (App syn chk) = App (substSyn i f syn) (substChk i f chk)