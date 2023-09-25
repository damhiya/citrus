module Syntax where

type Var = String

data VoidF t
  = VoidRec t

data UnitF t
  = Tt
  | UnitRec t t

data BoolF t
  = True
  | False
  | BoolRec t t t

data ProdF t
  = Pair t t
  | Fst t
  | Snd t
  | ProdRec t Var Var t

data SumF t
  = Inl t
  | Inr t
  | SumRec t Var t Var t

data Decl' t
  = Decl Var [Var] t

data FunF t
  = Lam Var t
  | LetRec [Decl' t] Term
  | App t t

data IntF t
  = IntLit Integer

data StrF t
  = StrLit String

data IOF t
  = Pure t
  | GetLine Var t
  | PutStr t Var t
  | Bind t Var t

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

type Decl = Decl' Term
