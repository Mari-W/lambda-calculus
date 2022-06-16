module Main where

import Dependently.Ast (DecExpr (..), Ident (..), InfExpr (..), Neutral (..), Result, Value (..))
import Dependently.Eval (eval)
import Dependently.Quote (quote)
import Dependently.Type (Env, typeOf)

dependently :: InfExpr -> Result DecExpr
dependently inf = do
  typeOf [(Global "Bool", VStar), (Global "False", VNeutral (NFree (Global "Bool")))] inf
  Right (quote (eval inf))

e :: InfExpr
-- e = (\x -> x :: (fa. x::* : x)) Bool False
e =
  App
    ( App
        ( Ann
            (Lam (InfExpr (Bound 0)))
            ( InfExpr
                ( Pi
                    (InfExpr Star)
                    (InfExpr (Bound 0))
                )
            )
        )
        (InfExpr (Free (Global "Bool")))
    )
    (InfExpr (Free (Global "False")))

main :: IO ()
main = case dependently e of
  Right e -> print e
  Left e -> print ("[error] " ++ e)