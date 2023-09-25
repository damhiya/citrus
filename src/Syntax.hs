module Syntax where

type Var = String

data VoidF t
  = VoidRec t
  deriving Show

data UnitF t
  = Tt
  | UnitRec t t
  deriving Show

data BoolF t
  = True
  | False
  | BoolRec t t t
  deriving Show

data ProdF t
  = Pair t t
  | Fst t
  | Snd t
  | ProdRec t Var Var t
  deriving Show

data SumF t
  = Inl t
  | Inr t
  | SumRec t Var t Var t
  deriving Show

data Decl' t
  = Decl Var [Var] t
  deriving Show

data FunF t
  = Lam Var t
  | LetRec [Decl' t] Term
  | App t t
  deriving Show

data IntF t
  = IntLit Integer
  deriving Show

data StrF t
  = StrLit String
  deriving Show

data IOPrimOp t
  = GetLine
  | GetInt
  | PutStr t
  deriving Show

data IOF t
  = Pure t
  | PrimIO (IOPrimOp t)
  | Bind t Var t
  deriving Show

data PrimOpF t
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

data Term
  = Var Var
  | Let Decl Term
  | Void (VoidF Term)
  | Unit (UnitF Term)
  | Bool (BoolF Term)
  | Prod (ProdF Term)
  | Sum (SumF Term)
  | Fun (FunF Term)
  | Int (IntF Term)
  | Str (StrF Term)
  | IO (IOF Term)
  | PrimOp (PrimOpF Term)
  deriving Show

type Decl = Decl' Term
