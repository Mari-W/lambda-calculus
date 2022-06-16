module Dependently.Quote where

import Dependently.Ast
  ( Binders,
    ChkExpr (..),
    Ident (Quote),
    SynExpr (..),
    Neutral (..),
    Value (..),
  )

quote :: Value -> ChkExpr
quote = quoteVal 0

neutralQuote :: Int -> Value
neutralQuote i = VNeutral (NFree (Quote i))

quoteVal :: Binders -> Value -> ChkExpr
quoteVal i (VClos f) = Lam (quoteVal (i + 1) (f (neutralQuote i)))
quoteVal i (VNeutral n) = SynExpr (quoteNeu i n)
quoteVal i VStar = SynExpr Star
quoteVal i (VPi v f) = SynExpr (Pi (quoteVal i v) (quoteVal (i + 1) (f (neutralQuote i))))

quoteNeu :: Binders -> Neutral -> SynExpr
quoteNeu i (NFree ident) = free i ident
quoteNeu i (NApp n v) = App (quoteNeu i n) (quoteVal i v)

free :: Binders -> Ident -> SynExpr
free i (Quote j) = Bound (i - j - 1)
free i ident = Free ident