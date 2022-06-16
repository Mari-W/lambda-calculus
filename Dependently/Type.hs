module Dependently.Type where

import Control.Monad ( unless )
import Dependently.Ast
  ( Binders,
    DecExpr (..),
    Ident (Local),
    InfExpr (..),
    Neutral (NFree),
    Result,
    Value (..),
  )
import Dependently.Eval (evalDec)
import Dependently.Quote (quote)

type Env = [(Ident, Value)]

raise :: String -> Result r
raise = Left

typeOf :: Env -> InfExpr -> Result Value
typeOf = typeInf 0

typeInf :: Binders -> Env -> InfExpr -> Result Value
typeInf i env Star = return VStar
typeInf i env (Pi dom dec) = do
  typeDec i env dom VStar
  typeDec (i + 1) ((Local i, evalDec dom []) : env) (subst (Free (Local i)) dec) VStar
  return VStar
typeInf i env (Ann dec ann) = do
  typeDec i env ann VStar
  let v = evalDec ann []
  typeDec i env dec v
  return v
typeInf i env (Free ident) = case lookup ident env of
  Just v -> return v
  _ -> raise ("unresolved: unknown ident " ++ show ident)
typeInf i env (App lam arg) =
  do
    w <- typeInf i env lam
    case w of
      VPi v f -> do
        typeDec i env arg v
        return (f (evalDec arg []))
      _ -> raise ("type mismatch: expected dependent function, got " ++ show (quote w))
typeInf i env (Bound j) = raise ("unresolved: unbound variable " ++ show j ++ " should have been substituted by now")

typeDec :: Binders -> Env -> DecExpr -> Value -> Result ()
typeDec i env (InfExpr inf) v = do
  w <- typeInf i env inf
  unless (quote v == quote w) (raise ("type mismatch: expected " ++ show (quote v) ++ ", got " ++ show (quote w)))
typeDec i env (Lam lam) (VPi v f) = typeDec (i + 1) ((Local i, v) : env) (subst (Free (Local i)) lam) (f (VNeutral (NFree (Local i))))
typeDec i env (Lam lam) v = raise ("type mismatch: expected function, got " ++ show (quote v))

subst :: InfExpr -> DecExpr -> DecExpr
subst = substDec 0

substDec :: Binders -> InfExpr -> DecExpr -> DecExpr
substDec i f (InfExpr inf) = InfExpr (substInf i f inf)
substDec i f (Lam lam) = Lam (substDec (i + 1) f lam)

substInf :: Binders -> InfExpr -> InfExpr -> InfExpr
substInf i f Star = Star
substInf i f (Pi v b) = Pi (substDec i f v) (substDec (i + 1) f b)
substInf i f (Ann dec v) = Ann (substDec i f dec) (substDec i f v)
substInf i f (Bound j) = if i == j then f else Bound j
substInf i f (Free id) = Free id
substInf i f (App inf dec) = App (substInf i f inf) (substDec i f dec)