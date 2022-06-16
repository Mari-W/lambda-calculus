module Simply.Type where

import Simply.Ast
  ( ChkExpr (..),
    Ident (Local),
    SynExpr (..),
    Kind (..),
    Type (..), Result, Binders
  )
import Control.Monad

type Env = [(Ident, Has)]

data Has
  = Kind Kind
  | Type Type
  deriving (Show)

raise :: String -> Result r
raise = Left

kind :: Env -> Type -> Kind -> Result ()
kind env (TFree ident) Star =
  case lookup ident env of
    Just (Kind Star) -> return ()
    _ -> raise ("unresolved: unknown ident " ++ show ident)
kind env (TFun lam arg) Star =
  do
    kind env lam Star
    kind env arg Star

typeOf :: Env -> SynExpr -> Result Type
typeOf = typeSyn 0

typeSyn :: Binders -> Env -> SynExpr -> Result Type
typeSyn i env (Ann chk ty) = do
  kind env ty Star
  typeChk i env chk ty
  return ty
typeSyn i env (Free ident) = case lookup ident env of
  Just (Type ty) -> return ty
  _ -> raise ("unresolved: unknown ident " ++ show ident)
typeSyn i env (App lam arg) =
  do
    ty <- typeSyn i env lam
    case ty of
      TFun from to -> do
        typeChk i env arg from
        return from
      _ -> raise ("type mismatch: expected function, got " ++ show ty)
typeSyn i env (Bound j) = raise ("unreachable: bound variable " ++ show j ++ " should have been substituted by now")

typeChk :: Binders -> Env -> ChkExpr -> Type -> Result ()
typeChk i env (SynExpr syn) ty = do
  ity <- typeSyn i env syn
  unless (ty == ity) (raise ("type mismatch: expected " ++ show ty ++ ", got " ++ show ity))
typeChk i env (Lam lam) (TFun from to) = typeChk (i + 1) ((Local i, Type from) : env) (substChk 0 (Free (Local i)) lam) to
typeChk i env (Lam lam) ty = raise ("type mismatch: expected function, got " ++ show ty)

substChk :: Binders -> SynExpr -> ChkExpr -> ChkExpr
substChk i f (SynExpr syn) = SynExpr (substSyn i f syn)
substChk i f (Lam lam) = Lam (substChk (i + 1) f lam)

substSyn :: Binders -> SynExpr -> SynExpr -> SynExpr
substSyn i f (Ann chk ty) = Ann (substChk i f chk) ty
substSyn i f (Bound j) = if i == j then f else Bound j
substSyn i f (Free id) = Free id
substSyn i f (App syn chk) = App (substSyn i f syn) (substChk i f chk)