module Dependently.Quote where

import Dependently.Ast
  ( Binders,
    DecExpr (..),
    Ident (Quote),
    InfExpr (..),
    Neutral (..),
    Value (..),
  )

quote :: Value -> DecExpr
quote = quoteVal 0

neutral :: Int -> Value
neutral i = VNeutral (NFree (Quote i))

quoteVal :: Binders -> Value -> DecExpr
quoteVal i (VClos f) = Lam (quoteVal (i + 1) (f (neutral i)))
quoteVal i (VNeutral n) = InfExpr (quoteNeu i n)
quoteVal i VStar = InfExpr Star
quoteVal i (VPi dom f) = InfExpr (Pi (quoteVal i dom) (quoteVal (i + 1) (f (neutral i))))

quoteNeu :: Binders -> Neutral -> InfExpr
quoteNeu i (NFree ident) = free i ident
quoteNeu i (NApp n v) = App (quoteNeu i n) (quoteVal i v)

free :: Binders -> Ident -> InfExpr
free i (Quote j) = Bound (i - j - 1)
free i ident = Free ident