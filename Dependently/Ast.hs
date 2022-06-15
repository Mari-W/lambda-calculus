module Dependently.Ast where

data InfExpr
  = Star
  | Pi DecExpr DecExpr
  | Ann DecExpr DecExpr
  | Bound Int
  | Free Ident
  | App InfExpr DecExpr
  deriving (Show, Eq)

data DecExpr
  = InfExpr InfExpr
  | Lam DecExpr
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