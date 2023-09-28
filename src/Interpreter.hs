module Interpreter where

import Evaluation qualified as E
import Syntax     qualified as S

interpret :: E.Env -> S.Term -> IO E.Value
interpret = \env t ->
  case E.eval env t of
    Right (E.Pure v) -> pure v
    Right (E.IOs E.GetLine ks) -> do
      s <- getLine
      go (E.Str s) ks
    Right (E.IOs E.GetInt ks) -> do
      n <- readLn
      go (E.Int n) ks
    Right (E.IOs (E.PutStr s) ks) -> do
      _ <- putStr s
      go E.Tt ks
    Right _ -> error "interpreter error : no IO"
    Left e  -> error ("eval error : " ++ show e)
  where
    go :: E.Value -> [(E.Env, S.Var, S.Term)] -> IO E.Value
    go v [] = pure v
    go v ((env, x, t):ks) = do
      v' <- interpret ((x,v):env) t
      go v' ks
