module Dependently.Syntax where

import Dependently.Ast
  ( ChkExpr (..),
    Ident (..),
    Neutral (..),
    SynExpr (..),
    Value (..),
  )

global :: String -> SynExpr
global s = Free (Global s)

global' :: String -> ChkExpr
global' s = chk (Free (Global s))

x, y, z :: ChkExpr
x = chk (Bound 0)
y = chk (Bound 1)
z = chk (Bound 2)

x', y', z' :: SynExpr
x' = Bound 0
y' = Bound 1
z' = Bound 2

star :: SynExpr
star = Star

star' :: ChkExpr
star' = chk Star

(--->) :: ChkExpr -> ChkExpr -> SynExpr
(--->) = Pi 
infixr 7 --->

(-->) :: ChkExpr -> ChkExpr -> ChkExpr
(-->) e e' = chk (Pi e e')
infixr 7 -->

chk :: SynExpr -> ChkExpr
chk = SynExpr

lam :: ChkExpr -> ChkExpr
lam = Lam 

(~~) :: ChkExpr -> ChkExpr -> SynExpr
(~~) = Ann
infix 6 ~~

(@@@) :: SynExpr -> ChkExpr -> ChkExpr
(@@@) e e' = chk (App e e')
infixl 5 @@@

(@@) :: SynExpr -> ChkExpr -> SynExpr
(@@) = App 
infixl 5 @@

