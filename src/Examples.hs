module Examples where

import Prelude hiding (Bool(..))
import Syntax
import Interpreter

ex1 :: Term
ex1 =
  Fun (LetRec [ Decl "even" ["n"]
                  (Bool (BoolRec
                    (PrimOp (IntEq (Var "n") (Int (IntLit 0))))
                    (Bool True)
                    (Fun (App
                            (Var "odd")
                            (PrimOp (IntSub (Var "n") (Int (IntLit 1))))))))
              , Decl "odd" ["n"]
                  (Bool (BoolRec
                    (PrimOp (IntEq (Var "n") (Int (IntLit 0))))
                    (Bool False)
                    (Fun (App
                            (Var "even")
                            (PrimOp (IntSub (Var "n") (Int (IntLit 1))))))))
              ]
              (IO (Bind (IO (PrimIO GetInt)) "m"
                    (IO (Pure (Fun (App (Var "even") (Var "m")))))
                  ))
      )

ex2 :: Term
ex2 =
  Fun (LetRec [ Decl "fact" ["n"]
                  (Bool (BoolRec
                    (PrimOp (IntEq (Var "n") (Int (IntLit 0))))
                    (Int (IntLit 1))
                    (PrimOp (IntMul (Var "n") (Fun (App
                            (Var "fact")
                            (PrimOp (IntSub (Var "n") (Int (IntLit 1))))))))))
              , Decl "loop" ["n"]
                  (Bool (BoolRec
                    (PrimOp (IntEq (Var "n") (Int (IntLit 0))))
                    (IO (Pure (Unit Tt)))
                    (IO (Bind (IO (PrimIO (PutStr
                                            (PrimOp (AppendStr (PrimOp (IntToStr (Fun (App (Var "fact") (Var "n")))))
                                                               (Str (StrLit "\n")))))))
                              "_"
                              (Fun (App
                                    (Var "loop")
                                    (PrimOp (IntSub (Var "n") (Int (IntLit 1))))))))))
              ]
              (IO (Bind (IO (PrimIO GetInt)) "m"
                    (Fun (App (Var "loop") (Var "m")))
                  ))
      )

go1 = interpret [] ex1
go2 = interpret [] ex2
