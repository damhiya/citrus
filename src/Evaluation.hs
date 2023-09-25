{-# LANGUAGE NoImplicitPrelude #-}

module Evaluation where

import Prelude hiding (Bool(..))
import Syntax qualified as S

data Value
  = Tt
  | True
  | False
  | Pair Value Value
  | Inl Value
  | Inr Value
  | Lam Env S.Var S.Term
  | LamRec Env [S.Decl] S.Decl
  | Int Integer
  | Str String
  | Pure Value
  | GetLine S.Var S.Term
  | PutStr Value S.Var S.Term

type Env = [(S.Var, Value)]

data EvalError
  = ErrScope
  | ErrVoidRec
  | ErrUnitRec
  | ErrBoolRec
  | ErrFst
  | ErrSnd
  | ErrProdRec
  | ErrSumRec
  | ErrLetRec
  | ErrApp
  | ErrBind
  | ErrIntEq
  | ErrIntLe
  | ErrIntAdd
  | ErrIntSub
  | ErrIntMul
  | ErrIntDiv
  | ErrIntMod
  | ErrIntToStr
  | ErrAppendStr

applyDecls :: [S.Decl] -> Env -> Env
applyDecls decls env
  = [(f, LamRec env decls (S.Decl f xs t)) | S.Decl f xs t <- decls] ++ env

lamExpand :: [S.Var] -> S.Term -> S.Term
lamExpand xs t = foldr (\x t -> S.Fun (S.Lam x t)) t xs

eval :: Env -> S.Term -> Either EvalError Value
eval env (S.Var x) =
  case lookup x env of
    Just v  -> Right v
    Nothing -> Left ErrScope
eval env (S.Let (S.Decl f xs t1) t2) = do
  v <- eval env (lamExpand xs t1)
  eval ((f,v):env) t2
eval env (S.Void (S.VoidRec t0)) = do
  _ <- eval env t0
  Left ErrVoidRec
eval env (S.Unit (S.Tt)) = Right Tt
eval env (S.Unit (S.UnitRec t0 t1)) = do
  v0 <- eval env t0
  case v0 of
    Tt -> eval env t1
    _  -> Left ErrUnitRec
eval env (S.Bool (S.True)) = Right True
eval env (S.Bool (S.False)) = Right False
eval env (S.Bool (S.BoolRec t0 t1 t2)) = do
  v0 <- eval env t0
  case v0 of
    True -> eval env t1
    False -> eval env t2
    _ -> Left ErrBoolRec
eval env (S.Prod (S.Pair t1 t2)) = do
  v1 <- eval env t1
  v2 <- eval env t2
  Right (Pair v1 v2)
eval env (S.Prod (S.Fst t0)) = do
  v0 <- eval env t0
  case v0 of
    Pair v1 v2 -> Right v1
    _ -> Left ErrFst
eval env (S.Prod (S.Snd t0)) = do
  v0 <- eval env t0
  case v0 of
    Pair v1 v2 -> Right v2
    _ -> Left ErrSnd
eval env (S.Prod (S.ProdRec t0 x1 x2 t)) = do
  v0 <- eval env t0
  case v0 of
    Pair v1 v2 -> eval ((x2, v2):(x1, v1):env) t
    _ -> Left ErrProdRec
eval env (S.Sum (S.Inl t)) = do
  v <- eval env t
  Right (Inl v)
eval env (S.Sum (S.Inr t)) = do
  v <- eval env t
  Right (Inr v)
eval env (S.Sum (S.SumRec t0 x t1 y t2)) = do
  v0 <- eval env t0
  case v0 of
    Inl vx -> eval ((x,vx):env) t1
    Inr vy -> eval ((y,vy):env) t2
    _ -> Left ErrSumRec
eval env (S.Fun (S.Lam x t)) =
  Right (Lam env x t)
eval env (S.Fun (S.LetRec decls t)) =
  if all (\(S.Decl _ xs _) -> not (null xs)) decls
     then eval (applyDecls decls env) t
     else Left ErrLetRec
eval env (S.Fun (S.App t0 t1)) = do
  v0 <- eval env t0
  case v0 of
    Lam env' x t -> do
      v <- eval env t1
      eval ((x,v):env') t
    LamRec env' decls (S.Decl f [] t) -> Left ErrApp
    LamRec env' decls (S.Decl f (x:xs) t) -> do
      v <- eval env t1
      eval ((x,v) : applyDecls decls env') (lamExpand xs t)
    _ -> Left ErrApp
eval env (S.Int (S.IntLit n)) = Right (Int n)
eval env (S.Str (S.StrLit s)) = Right (Str s)
eval env (S.IO (S.Pure t)) = do
  v <- eval env t
  Right (Pure v)
eval env (S.IO (S.GetLine x t)) = Right (GetLine x t)
eval env (S.IO (S.PutStr t1 x t2)) = do
  v1 <- eval env t1
  Right (PutStr v1 x t2)
eval env (S.IO (S.Bind t0 x t)) = do
  v0 <- eval env t0
  case v0 of
    Pure v -> eval ((x,v):env) t
    GetLine y t1 -> Right (GetLine y (S.IO (S.Bind t1 x t)))
    PutStr v1 y t2 -> Right (PutStr v1 y (S.IO (S.Bind t2 x t)))
    _ -> Left ErrBind
eval env (S.PrimOp pt) = primop pt
  where
    primop (S.IntEq t1 t2) = do
      v1 <- eval env t1
      v2 <- eval env t2
      case (v1, v2) of
        (Int n1, Int n2) -> Right (if n1 == n2 then True else False)
        _ -> Left ErrIntEq
    primop (S.IntLe t1 t2) = do
      v1 <- eval env t1
      v2 <- eval env t2
      case (v1, v2) of
        (Int n1, Int n2) -> Right (if n1 <= n2 then True else False)
        _ -> Left ErrIntLe
    primop (S.IntAdd t1 t2) = do
      v1 <- eval env t1
      v2 <- eval env t2
      case (v1, v2) of
        (Int n1, Int n2) -> Right (Int (n1 + n2))
        _ -> Left ErrIntAdd
    primop (S.IntSub t1 t2) = do
      v1 <- eval env t1
      v2 <- eval env t2
      case (v1, v2) of
        (Int n1, Int n2) -> Right (Int (n1 - n2))
        _ -> Left ErrIntSub
    primop (S.IntMul t1 t2) = do
      v1 <- eval env t1
      v2 <- eval env t2
      case (v1, v2) of
        (Int n1, Int n2) -> Right (Int (n1 * n2))
        _ -> Left ErrIntMul
    primop (S.IntDiv t1 t2) = do
      v1 <- eval env t1
      v2 <- eval env t2
      case (v1, v2) of
        (Int n1, Int n2) -> Right (Int (n1 `div` n2))
        _ -> Left ErrIntDiv
    primop (S.IntMod t1 t2) = do
      v1 <- eval env t1
      v2 <- eval env t2
      case (v1, v2) of
        (Int n1, Int n2) -> Right (Int (n1 `mod` n2))
        _ -> Left ErrIntMod
    primop (S.IntToStr t) = do
      v <- eval env t
      case v of
        Int n -> Right (Str (show n))
        _ -> Left ErrIntToStr
    primop (S.AppendStr t1 t2) = do
      v1 <- eval env t1
      v2 <- eval env t2
      case (v1, v2) of
        (Str s1, Str s2) -> Right (Str (s1 ++ s2))
        _ -> Left ErrAppendStr
