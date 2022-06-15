module Main where

import Dependently.Ast (DecExpr, InfExpr, Result)
import Dependently.Eval (eval)
import Dependently.Quote (quote)
import Dependently.Type (Env, typeOf)
import Simply.Ast (DecExpr, InfExpr, Result)
import Simply.Eval (eval)
import Simply.Quote (quote)
import Simply.Type (Env, typeOf)

simply :: Simply.Ast.InfExpr -> Simply.Ast.Result Simply.Ast.DecExpr
simply inf = do
  Simply.Type.typeOf [] inf
  Right (Simply.Quote.quote (Simply.Eval.eval inf))

dependently :: Dependently.Ast.InfExpr -> Dependently.Ast.Result Dependently.Ast.DecExpr
dependently inf = do
  Dependently.Type.typeOf [] inf
  Right (Dependently.Quote.quote (Dependently.Eval.eval inf))

main :: IO ()
main = print ()
