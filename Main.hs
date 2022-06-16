module Main where

import Dependently.Ast (ChkExpr (..), Ident (..), Neutral (..), Result, SynExpr (..), Value (..))
import Dependently.Eval (eval)
import Dependently.Quote (quote)
import Dependently.Syntax
  ( global',
    lam,
    star',
    chk,
    x,
    y,
    (--->),
    (-->),
    (@@),
    (~~),
  )
import Dependently.Type (Env, typeOf)

dependently :: SynExpr -> Result (ChkExpr, ChkExpr)
dependently syn = do
  let env = [(Global "Bool", VStar), (Global "False", VNeutral (NFree (Global "Bool")))]
  ty <- typeOf env syn
  Right (quote (eval syn), quote ty)

-- pid := (lam a -> (lam x -> x) :: (fa a::* : a -> a))
-- -> pid :: (fa x::*) (y::x).x
pid = lam (lam x) ~~ (star' --> (x --> y))

-- e := (lam a -> (lam x -> x) :: (fa x::* : x)) Bool False
-- -> False :: Bool
e = (pid @@ global' "Bool") @@ global' "False"

main :: IO ()
main = case dependently e of
  Right (e, v) -> print (show e ++ " :: " ++ show v)
  Left e -> print ("[error] " ++ e)