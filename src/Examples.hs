module Examples where

import Evaluation  qualified as E
import Interpreter
import Prelude     hiding (Bool (..))
import Syntax

ex1 :: Term
ex1 =
  LetRec [ Decl "even" ["n"]
                  (BoolRec
                    (PrimOp (IntEq (Var "n") (IntLit 0)))
                    True
                    (App
                            (Var "odd")
                            (PrimOp (IntSub (Var "n") (IntLit 1)))))
              , Decl "odd" ["n"]
                  (BoolRec
                    (PrimOp (IntEq (Var "n") (IntLit 0)))
                    False
                    (App
                            (Var "even")
                            (PrimOp (IntSub (Var "n") (IntLit 1)))))
              ]
              (Bind (PrimIO GetInt) "m"
                    (Pure (App (Var "even") (Var "m")))
                  )

ex2 :: Term
ex2 =
  LetRec [ Decl "fact" ["n"]
                  (BoolRec
                    (PrimOp (IntEq (Var "n") (IntLit 0)))
                    (IntLit 1)
                    (PrimOp (IntMul (Var "n") (App
                            (Var "fact")
                            (PrimOp (IntSub (Var "n") (IntLit 1)))))))
              , Decl "loop" ["n"]
                  (BoolRec
                    (PrimOp (IntEq (Var "n") (IntLit 0)))
                    (Pure Tt)
                    (Bind (PrimIO (PutStr
                                            (PrimOp (AppendStr (PrimOp (IntToStr (App (Var "fact") (Var "n"))))
                                                               (StrLit "\n")))))
                              "_"
                              (App
                                    (Var "loop")
                                    (PrimOp (IntSub (Var "n") (IntLit 1))))))
              ]
              (Bind (PrimIO GetInt) "m"
                    (App (Var "loop") (Var "m"))
                  )

go1, go2 :: IO E.Value
go1 = interpret [] ex1
go2 = interpret [] ex2
