import Data.Vect as V
import Prelude.List as L
import Language.Reflection.Utils

%language ElabReflection


--data B = T | F
%runElab
  (do declareDatatype $
        Declare `{{B}} [] `(Type)
      defineDatatype $
        DefineDatatype `{{B}}
          [ Constructor `{{T}} [] (Var `{{B}})
            , Constructor `{{F}} [] (Var `{{B}})
            ])
            
            
imp : TTName -> Raw -> FunArg
imp n t = MkFunArg n t Implicit Erased
exp : TTName -> Raw -> FunArg
exp n t = MkFunArg n t Explicit NotErased

-- equivalent to: append : Vect n a -> Vect m a -> Vect (n + m) a
-- does not define append
appendDecl : TyDecl
appendDecl =
  Declare
    `{{append}}
    [ imp `{{a}} `(Type)
      , imp `{{n}} `(Nat)
      , imp `{{m}} `(Nat)
      , exp `{{xs}}
        `(Vect ~(Var `{{n}}) ~(Var `{{a}}))
      , exp `{{ys}}
        `(Vect ~(Var `{{m}}) ~(Var `{{a}}))
    ]
    `(Vect (plus ~(Var `{{n}})
                 ~(Var `{{m}}))
                 ~(Var `{{a}}))


-- %runElab 
--   (do
--     (declareType appendDecl)
-- --    the (Elab ()) debug
--     )


-- xxx : Nat
-- xxx = let vd = Declare `{{val}} [] `(Nat)
--           x = S Z
--         in %runElab (do declareType (Declare `{{val}} [] `(Nat))
--                         fill `(Z)
--                         solve)

-- xs : List Nat
-- xs = let x = S Z
--   --valDecl = Declare `{{val}} [] `(Nat)
--      in [%runElab (do declareType (Declare `{{val}} [] `(Nat))
--                       --focus `{{val}}
--                       fill `(Z)
--                       solve),
--          x + val]

-- %runElab defineFunction $ DefineFun `{{val}} [(MkFunClause (RConstant VoidType) (RConstant (BI 42)))]

-- e0 : Elab () -> Elab ()
-- e0 x = x

-- envToErrorReport : (List (TTName, Binder TT)) -> List ErrorReportPart
-- envToErrorReport xs = map ?envToErrorReport_rhs xs



-- %runElab do env <- getEnv
--             e0 $ debugMessage [(TextPart "foo")]
            

         
quicksort : List Nat -> List Nat
quicksort [] = []
quicksort (x::xs) =
  quicksort (filter (< x) xs) ++
  x :: quicksort (filter (>= x) xs)

-- processClauses : Nat -> (List (FunClause TT)) -> Elab (List (FunClause Raw, ConstructorDefn))

-- bc : (inFun, outFun, accPred : TTName) -> Elab ()
-- bc inFun outFun accPred = do
--   (inFun', Ref, inTy) <- lookupTyExact inFun
--      | _ => fail [TextPart "Not a function"]
--   (_, args, res) <- lookupArgsExact inFun'
--   declareDatatype $ Declare accPred args `(Type)
--   let accArg = MkFunArg `{{acc}}
--                 (mkApp (Var accPred)
--                    (map (Var . name) args))
--                 Explicit
--                 NotErased
--   declareType $
--     Declare outFun (args ++ [accArg]) res
--   DefineFun _ oldClauses <- lookupFunDefnExact inFun'
--   (newClauses, ctors) <-
--     Prelude.List.unzip <$> processClauses Z oldClauses
--   defineDatatype $ DefineDatatype accPred ctors
--   defineFunction $ DefineFun outFun newClauses
  
