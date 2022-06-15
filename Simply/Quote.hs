module Simply.Quote where

import Simply.Ast
  ( Binders,
    DecExpr (..),
    Ident (Quote),
    InfExpr (App, Bound, Free),
    Neutral (..),
    Value (..),
  )

quote :: Value -> DecExpr
quote = quoteVal 0

quoteVal :: Binders -> Value -> DecExpr
quoteVal i (VClos f) = Lam (quoteVal (i + 1) (f (VNeutral (NFree (Quote i)))))
quoteVal i (VNeutral n) = InfExpr (quoteNeu i n)

quoteNeu :: Binders -> Neutral -> InfExpr
quoteNeu i (NFree ident) = free i ident
quoteNeu i (NApp n v) = App (quoteNeu i n) (quoteVal i v)

free :: Binders -> Ident -> InfExpr
free i (Quote j) = Bound (i - j - 1)
free i ident = Free ident