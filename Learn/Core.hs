module Core where

import Idris.Core.TT
import Idris.Core.Evaluate
import Data.Text
import Idris.Core.Typecheck(check)

--import Idris.Elab.Utils
import Idris.Reflection
import Idris.Core.Elaborate
import Idris.AbsSyntaxTree (initEState)

t1 = check initContext [] (Var $ UN $ pack "foo")
--Error: NoSuchVariable foo

fortytwo = (RConstant (I 42))

t2 = check initContext [] fortytwo
--(42,Int)


f1 = RBind (UN $ pack "foo") (Lam { binderCount = RigW, binderTy = RConstant (AType (ATInt ITNative)) }) (Var (UN $ pack "foo"))

t3 = check initContext [] f1

t4 = check initContext [] (RApp f1 fortytwo)

fBad = RBind (UN $ pack "foo") (Lam { binderCount = RigW, binderTy = RConstant StrType }) (Var (UN $ pack "foo"))

t5 = check initContext [] (RApp fBad fortytwo)
--Error: CantConvert Int and String  in []

t6 = let (OK (tt,_)) = t4
         x = runElab initEState (reifyRaw tt) (initElaborator (sMN 0 "hole") internalNS initContext emptyContext 0 Erased)
     in case x of
          (OK (v,_)) -> (show v)
          (Error v) -> (show v)

