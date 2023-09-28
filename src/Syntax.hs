module Syntax where

type Var = String

data PrimIO t
  = GetLine
  | GetInt
  | PutStr t
  deriving Show

data PrimOp t
  = IntEq t t
  | IntLe t t
  | IntAdd t t
  | IntSub t t
  | IntMul t t
  | IntDiv t t
  | IntMod t t
  | IntToStr t
  | AppendStr t t
  deriving Show

data Decl
  = Decl Var [Var] Term
  deriving Show

data Term
  = Var Var
  -- empty
  | EmptyRec Term
  -- unit
  | Tt
  | UnitRec Term Term
  -- bool
  | True
  | False
  | BoolRec Term Term Term
  -- product
  | Pair Term Term
  | Fst Term
  | Snd Term
  | ProdRec Term Var Var Term
  -- sum
  | Inl Term
  | Inr Term
  | SumRec Term Var Term Var Term
  -- function
  | Lam Var Term
  | App Term Term
  -- let
  | Let Decl Term
  | LetRec [Decl] Term
  -- integer
  | IntLit Integer
  -- string
  | StrLit String
  -- io
  | Pure Term
  | PrimIO (PrimIO Term)
  | Bind Term Var Term
  -- primitive operations
  | PrimOp (PrimOp Term)
  deriving Show
