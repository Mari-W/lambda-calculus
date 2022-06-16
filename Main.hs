module Main where

import Dependently.Ast (ChkExpr (..), Ident (..), Neutral (..), Result, SynExpr (..), Value (..))
import Dependently.Eval (eval)
import Dependently.Quote (quote)
import Dependently.Type (Env, typeOf)

dependently :: SynExpr -> Result ChkExpr
dependently syn = do
  let env = [(Global "Bool", VStar), (Global "False", VNeutral (NFree (Global "Bool")))]
  typeOf env syn
  Right (quote (eval syn))

dId :: SynExpr
-- dId := (\x -> x :: (fa. x::* : x))
-- -> dId :: (fa. x::*) (y::x).x
dId =
  Ann
    (Lam (SynExpr (Bound 0)))
    ( SynExpr
        ( Pi
            (SynExpr Star)
            (SynExpr (Bound 0))
        )
    )

e :: SynExpr
-- e := (\x -> x :: (fa. x::* : x)) Bool False
-- -> False :: Bool
e =
  App
    ( App
        dId
        (SynExpr (Free (Global "Bool")))
    )
    (SynExpr (Free (Global "False")))


main :: IO ()
main = case dependently dId of
  Right e -> print e
  Left e -> print ("[error] " ++ e)

