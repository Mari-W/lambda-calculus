module Simply.Ast where

data Kind
  = Star
  deriving (Show)

data Type
  = TFree Ident
  | TFun Type Type
  deriving (Show, Eq)

data SynExpr
  = Ann ChkExpr Type
  | Bound Int
  | Free Ident
  | App SynExpr ChkExpr
  deriving (Show, Eq)

data ChkExpr
  = SynExpr SynExpr
  | Lam ChkExpr
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