module Dependently.Ast where

data SynExpr
  = Star
  | Pi ChkExpr ChkExpr
  | Ann ChkExpr ChkExpr
  | Bound Int
  | Free Ident
  | App SynExpr ChkExpr
  deriving (Show, Eq)

data ChkExpr
  = SynExpr SynExpr
  | Lam ChkExpr
  deriving (Show, Eq)

data Value
  = VStar
  | VPi Value (Value -> Value)
  | VClos (Value -> Value)
  | VNeutral Neutral

data Neutral
  = NFree Ident
  | NApp Neutral Value

data Ident
  = Global String
  | Local Int
  | Quote Int
  deriving (Show, Eq)

type Binders = Int

type Result r = Either String r