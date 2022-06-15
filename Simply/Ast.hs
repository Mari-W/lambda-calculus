module Simply.Ast where

data Kind
  = Star
  deriving (Show)

data Type
  = TFree Ident
  | TFun Type Type
  deriving (Show, Eq)

data InfExpr
  = Ann DecExpr Type
  | Bound Int
  | Free Ident
  | App InfExpr DecExpr
  deriving (Show, Eq)

data DecExpr
  = InfExpr InfExpr
  | Lam DecExpr
  deriving (Show, Eq)

data Value
  = VClos (Value -> Value)
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