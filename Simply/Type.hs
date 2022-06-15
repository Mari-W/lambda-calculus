module Simply.Type where

import Simply.Ast
  ( DecExpr (..),
    Ident (Local),
    InfExpr (..),
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

typeOf :: Env -> InfExpr -> Result Type
typeOf = typeInf 0

typeInf :: Binders -> Env -> InfExpr -> Result Type
typeInf i env (Ann dec ty) = do
  kind env ty Star
  typeDec i env dec ty
  return ty
typeInf i env (Free ident) = case lookup ident env of
  Just (Type ty) -> return ty
  _ -> raise ("unresolved: unknown ident " ++ show ident)
typeInf i env (App lam arg) =
  do
    ty <- typeInf i env lam
    case ty of
      TFun from to -> do
        typeDec i env arg from
        return from
      _ -> raise ("type mismatch: expected function, got " ++ show ty)
typeInf i env (Bound j) = raise ("unreachable: bound variable " ++ show j ++ " should have been substituted by now")

typeDec :: Binders -> Env -> DecExpr -> Type -> Result ()
typeDec i env (InfExpr inf) ty = do
  ity <- typeInf i env inf
  unless (ty == ity) (raise ("type mismatch: expected " ++ show ty ++ ", got " ++ show ity))
typeDec i env (Lam lam) (TFun from to) = typeDec (i + 1) ((Local i, Type from) : env) (substDec 0 (Free (Local i)) lam) to
typeDec i env (Lam lam) ty = raise ("type mismatch: expected function, got " ++ show ty)

substDec :: Binders -> InfExpr -> DecExpr -> DecExpr
substDec i f (InfExpr inf) = InfExpr (substInf i f inf)
substDec i f (Lam lam) = Lam (substDec (i + 1) f lam)

substInf :: Binders -> InfExpr -> InfExpr -> InfExpr
substInf i f (Ann dec ty) = Ann (substDec i f dec) ty
substInf i f (Bound j) = if i == j then f else Bound j
substInf i f (Free id) = Free id
substInf i f (App inf dec) = App (substInf i f inf) (substDec i f dec)