module Simply.Quote where

import Simply.Ast
  ( Binders,
    ChkExpr (..),
    Ident (Quote),
    SynExpr (App, Bound, Free),
    Neutral (..),
    Value (..),
  )

quote :: Value -> ChkExpr
quote = quoteVal 0

quoteVal :: Binders -> Value -> ChkExpr
quoteVal i (VClos f) = Lam (quoteVal (i + 1) (f (VNeutral (NFree (Quote i)))))
quoteVal i (VNeutral n) = SynExpr (quoteNeu i n)

quoteNeu :: Binders -> Neutral -> SynExpr
quoteNeu i (NFree ident) = free i ident
quoteNeu i (NApp n v) = App (quoteNeu i n) (quoteVal i v)

free :: Binders -> Ident -> SynExpr
free i (Quote j) = Bound (i - j - 1)
free i ident = Free ident